#include "asdf.h"
#include "m_pd.h"
#include <math.h>  /* for ceil() */

/* class */
static t_class *asdf_tilde_class;

/* dataspace */
typedef struct _asdf_tilde {
  t_object  x_obj;  /* required, has to be the first entry */
  uint32_t blocksize;
  uint32_t signal_outlets;
  size_t dsp_data_size;
  t_int *dsp_data;
  AsdfScene *scene;
  AsdfTransform *transforms;
  uint32_t file_sources;
  uint32_t live_sources;
  /* writable memory to be passed to asdf_get_audio_data()
     if there are more file sources than outlets;
     a single (reusable) channel is enough */
  float *dummy_source_data;
  bool rolling;
  bool seeking;
  uint64_t frame;
} t_asdf_tilde;

void reset_dsp_data(t_asdf_tilde *x)
{
  /* first pointer for t_asdf_tilde object ... */
  uint32_t required_size = 1;
  /* ... the following pointers for file sources ... */
  required_size += x->file_sources;
  /* ... and the rest for unused outlets */
  if (x->file_sources < x->signal_outlets) {
    required_size += x->signal_outlets - x->file_sources;
  }
  /* NB: dsp_data is never shrunk, only re-allocated if more space is needed */
  if (x->dsp_data_size < required_size) {
    freebytes(x->dsp_data, x->dsp_data_size * sizeof(t_int));
    x->dsp_data_size = required_size;
    x->dsp_data = getbytes(x->dsp_data_size * sizeof(t_int));
    x->dsp_data[0] = (t_int)x;
  }
}

/* f: maximum number of (file) sources */
void *asdf_tilde_new(t_floatarg f)
{
  t_asdf_tilde *x = (t_asdf_tilde *)pd_new(asdf_tilde_class);
  float arg = f;
  x->signal_outlets = f;
  if (arg < 0.0 || arg != x->signal_outlets) {
    error("asdf~: argument has to be a non-negative integer");
    return NULL;
  }
  x->blocksize = sys_getblksize();
  reset_dsp_data(x);

  /* first outlet for messages */
  outlet_new(&x->x_obj, NULL);
  /* further outlets for source signals */
  for (uint32_t i = 0; i < x->signal_outlets; i++) {
    /* NB: we don't bother to store the resulting outlet pointers */
    outlet_new(&x->x_obj, &s_signal);
  }
  return (void *)x;
}

void clear_scene(t_asdf_tilde *x)
{
  if (x->file_sources > x->signal_outlets) {
    freebytes(x->dummy_source_data, x->blocksize * sizeof(float));
  }
  if (x->transforms) {
    freebytes(
        x->transforms,
        (1 + x->file_sources + x->live_sources) * sizeof(AsdfTransform));
    x->transforms = NULL;
  }
  asdf_scene_free(x->scene);
  x->scene = NULL;
  x->file_sources = 0;
  x->live_sources = 0;
  x->rolling = false;
  x->seeking = false;
  x->frame = 0;
}

void asdf_tilde_free(t_asdf_tilde *x)
{
  clear_scene(x);
  freebytes(x->dsp_data, x->dsp_data_size * sizeof(t_int));
  /* NB: we don't free inlets/outlets because we hope Pd does it for us */
}

/* s: ASDF scene file name */
void asdf_tilde_open(t_asdf_tilde *x, t_symbol *s)
{
  if (x->scene) {
    /* deactivate all active sources */
    size_t sources = x->file_sources + x->live_sources;
    for (size_t source_number = 1; source_number <= sources; source_number++) {
      t_atom argv[3];
      if (x->transforms[source_number].active) {
        SETFLOAT(argv, source_number);
        SETSYMBOL(argv + 1, gensym("active"));
        SETFLOAT(argv + 2, false);
        outlet_anything(x->x_obj.ob_outlet, gensym("src"), 3, argv);
      }
    }
    clear_scene(x);
  }
  post("asdf~: opening ASDF scene \"%s\"", s->s_name);
  /* TODO: reasonable default values? */
  /* TODO: ability to overwrite default values? */
  float sleep_time = 0.1;  /* seconds */
  float buffer_time = 2.0;  /* seconds */
  float samplerate = sys_getsr();
  uint32_t buffer_blocks = ceil(buffer_time * samplerate / (float)x->blocksize);
  uint64_t usleeptime = sleep_time * 1000.0f * 1000.0f;
  x->scene = asdf_scene_new(
      s->s_name,
      samplerate,
      x->blocksize,
      buffer_blocks,
      usleeptime
  );
  if (!x->scene) {
    pd_error(&x->x_obj, "asdf~: %s", asdf_last_error());
    return;
  }

  /* See asdf_tilde_seek() */
  if (!asdf_seek(x->scene, 0)) {
    x->seeking = true;
  }

  x->file_sources = asdf_file_sources(x->scene);
  /*
  x->live_sources = asdf_live_sources(x->scene);
  */

  post("asdf~: opened scene with %u file source(s)", x->file_sources);
  if (x->signal_outlets < x->file_sources) {
    /* NB: this is just a warning, available outlets will receive audio data */
    pd_error(
        &x->x_obj,
        "asdf~: fewer signal outlets (%u) than file sources (%u)",
        x->signal_outlets,
        x->file_sources);
  }

  reset_dsp_data(x);
  if (x->file_sources > x->signal_outlets) {
    x->dummy_source_data = getbytes(x->blocksize * sizeof(float));
  }
  x->transforms = getbytes(
      (1 + x->file_sources + x->live_sources) * sizeof(AsdfTransform));

  /* TODO: send number of sources to outlet */
  /* TODO: send id/name/model of sources to outlet */
  /*       asdf_get_sourceinfo()/asdf_sourceinfo_free() */
}

/* f: 1 for play, 0 for pause */
void asdf_tilde_float(t_asdf_tilde *x, t_floatarg f)
{
  float arg = f;
  if (arg == 0.0) {
    x->rolling = false;
  } else if (arg == 1.0) {
    if (!x->scene) {
      pd_error(&x->x_obj, "asdf~: start requested with no prior 'open'");
      return;
    }
    x->rolling = true;
  } else {
    pd_error(&x->x_obj, "asdf~: use 1 to play and 0 to pause, not %f", arg);
  }
}

/* f: seek position in seconds */
void asdf_tilde_seek(t_asdf_tilde *x, t_floatarg f)
{
  if (!x->scene) {
    pd_error(&x->x_obj, "asdf~: seek requested with no prior 'open'");
    return;
  }
  float seek_seconds = f;
  if (seek_seconds < 0) {
    pd_error(
        &x->x_obj,
        "asdf~: seek requires a non-negative argument, not %f",
        seek_seconds);
    return;
  }
  x->frame = seek_seconds * sys_getsr();
  if (!asdf_seek(x->scene, x->frame)) {
    x->seeking = true;
    /* NB: seeking will be continued in the DSP function */
  }
}

void send_transform(
    size_t source_number,
    AsdfTransform *current,
    AsdfTransform *previous,
    t_outlet *outlet,
    t_atom *argv)
{
  size_t offset;
  t_symbol *sym;
  if (source_number) {
    SETFLOAT(argv, source_number);
    offset = 1;
    sym = gensym("src");
    if (current->active != previous->active) {
      SETSYMBOL(argv + 1, gensym("active"));
      SETFLOAT(argv + 2, current->active);
      outlet_anything(outlet, sym, 3, argv);
    }
  } else {
    offset = 0;
    sym = gensym("ref");
    /* NB: we assume the reference to be always active */
  }

  if (current->active) {
    if (!previous->active
      || current->pos[0] != previous->pos[0]
      || current->pos[1] != previous->pos[1]
      || current->pos[2] != previous->pos[2])
    {
      SETSYMBOL(argv + offset, gensym("pos"));
      SETFLOAT(argv + offset + 1, current->pos[0]);
      SETFLOAT(argv + offset + 2, current->pos[1]);
      SETFLOAT(argv + offset + 3, current->pos[2]);
      outlet_anything(outlet, sym, offset + 4, argv);
      previous->pos[0] = current->pos[0];
      previous->pos[1] = current->pos[1];
      previous->pos[2] = current->pos[2];
    }

    /* TODO: quat */
    /* TODO: switch for quaternion vs. angles? */
    /* TODO: other parts of transform (vol, ...) */
  }
  previous->active = current->active;
}

/* send current positions and rotations to outlet */
void asdf_tilde_bang(t_asdf_tilde *x)
{
  if (!x->scene) {
    return;
  }
  /* TODO: send x->rolling? */

  /* NB: longest message: src i quat x y z w */
  /*                          0  1   2 3 4 5 */
  t_atom argv[6];

  AsdfTransform current;

  /* TODO: option for getting values whether or not they have changed? */

  for (size_t idx = 0; idx < x->file_sources + x->live_sources; idx++) {
    current = asdf_get_source_transform(x->scene, idx, x->frame);

    /* NB: one-based source numbers per SSR convention */
    AsdfTransform *previous = &x->transforms[idx + 1];
    send_transform(idx + 1, &current, previous, x->x_obj.ob_outlet, argv);
  }
  current = asdf_get_reference_transform(x->scene, x->frame);
  /* NB: source number 0 means reference */
  AsdfTransform *previous = &x->transforms[0];
  send_transform(0, &current, previous, x->x_obj.ob_outlet, argv);

  /* TODO: send frame number? */
}

t_int *asdf_tilde_perform(t_int *w)
{
  /* NB: 1-based indexing! */
  t_asdf_tilde *x = (t_asdf_tilde *)w[1];
  uint32_t blocksize = x->blocksize;
  if (x->scene) {
    bool rolling = x->rolling;
    if (x->seeking) {
      if (asdf_seek(x->scene, x->frame)) {
        x->seeking = false;
      } else {
        /* NB: seeking is not possible while rolling */
        rolling = false;
      }
    }
    if (asdf_get_audio_data(x->scene, (t_sample **)(w + 2), rolling)) {
      if (rolling) {
        x->frame += blocksize;
      }
    } else {
      pd_error(&x->x_obj, "asdf~: %s", asdf_last_error());
    }
  }
  uint32_t i = x->file_sources;
  /* fill remaining outlets with zeros */
  for (; i < x->signal_outlets; i++) {
    t_sample *target = (t_sample *)w[i + 2];
    for (uint32_t j = 0; j < blocksize; j++) {
      /* TODO: use memset()? */
      target[j] = 0.0;
    }
  }
  return w + i + 2;
}

void asdf_tilde_dsp(t_asdf_tilde *x, t_signal **sp)
{
  /* dsp_data[0]: pointer to the t_asdf_tilde object */
  /* dsp_data[1..file_sources+1]: storage for audio data */
  /* dsp_data[file_sources+1..]: unused outlets, to be filled with zeros */
  uint32_t i = 0;
  for (; i < x->file_sources; i++) {
    if (i < x->signal_outlets) {
      x->dsp_data[i + 1] = (t_int)sp[i]->s_vec;
    } else {
      /* NB: A single channel is reused multiple times if necessary */
      x->dsp_data[i + 1] = (t_int)x->dummy_source_data;
    }
  }
  for (; i < x->signal_outlets; i++) {
    x->dsp_data[i + 1] = (t_int)sp[i]->s_vec;
  }
  if (!x->signal_outlets || x->blocksize == (uint32_t)sp[0]->s_n) {
    dsp_addv(asdf_tilde_perform, i + 1, x->dsp_data);
  } else {
    /* TODO: is there a way to avoid this? */
    pd_error(
        &x->x_obj,
        "asdf~ must run with the system block size %u, not %i",
        x->blocksize,
        sp[0]->s_n);
  }
}

/* library setup */
void asdf_tilde_setup(void)
{
  asdf_tilde_class = class_new(
      gensym("asdf~"),  /* symbolic name */
      (t_newmethod)asdf_tilde_new,  /* ctor */
      (t_method)asdf_tilde_free,  /* dtor */
      sizeof(t_asdf_tilde),  /* size of dataspace */
      CLASS_DEFAULT,  /* graphical representation */
      /* up to 6 arguments (A_DEFFLOAT/A_DEFSYMBOL/A_GIMME): */
      A_DEFFLOAT,
      0  /* end of arguments */
  );

  class_addmethod(asdf_tilde_class,
      (t_method)asdf_tilde_open, gensym("open"), A_DEFSYMBOL, 0);

  class_addfloat(asdf_tilde_class, asdf_tilde_float);

  class_addmethod(asdf_tilde_class,
      (t_method)asdf_tilde_seek, gensym("seek"), A_DEFFLOAT, 0);

  class_addbang(asdf_tilde_class, asdf_tilde_bang);

  class_addmethod(asdf_tilde_class,
      (t_method)asdf_tilde_dsp, gensym("dsp"), A_CANT, 0);
}
