#include "asdf.h"
#include "m_pd.h"
#include <math.h>  /* for ceil() */
#include <stdio.h>  /* for snprintf() */

/* class */
static t_class *asdf_tilde_class;

/* dataspace */
typedef struct _asdf_tilde {
  t_object  x_obj;  /* required, has to be the first entry */
  t_canvas* x_canvas;
  uint32_t blocksize;
  uint32_t signal_outlets;
  float **outlet_ptrs;
  AsdfScene *scene;
  AsdfTransform *transforms;
  AsdfSourceInfo **sourceinfo;
  uint32_t file_sources;
  uint32_t live_sources;
  float **file_source_ptrs;
  /* writable memory to be used in file_source_ptrs
     if there are more file sources than outlets;
     a single (reusable) channel is enough */
  float *dummy_source_data;
  bool rolling;
  bool seeking;
  uint64_t frame;
  bool send_source_ids;
} t_asdf_tilde;

/* f: maximum number of (file) sources */
void *asdf_tilde_new(t_floatarg f)
{
  t_asdf_tilde *x = (t_asdf_tilde *)pd_new(asdf_tilde_class);
  float arg = f;
  x->signal_outlets = f;
  if (arg < 0.0 || arg != x->signal_outlets) {
    pd_error(NULL, "asdf~: argument has to be a non-negative integer");
    return NULL;
  }
  x->x_canvas = canvas_getcurrent();
  x->blocksize = sys_getblksize();

  /* first outlet for messages */
  outlet_new(&x->x_obj, NULL);
  /* further outlets for source signals */
  for (uint32_t i = 0; i < x->signal_outlets; i++) {
    /* NB: we don't bother to store the resulting outlet pointers */
    outlet_new(&x->x_obj, &s_signal);
  }
  x->outlet_ptrs = getbytes(x->signal_outlets * sizeof(float *));
  return (void *)x;
}

void clear_scene(t_asdf_tilde *x)
{
  freebytes(x->file_source_ptrs, x->file_sources * sizeof(float *));
  if (x->file_sources > x->signal_outlets) {
    freebytes(x->dummy_source_data, x->blocksize * sizeof(float));
  }
  if (x->transforms) {
    freebytes(
        x->transforms,
        (1 + x->file_sources + x->live_sources) * sizeof(AsdfTransform));
    x->transforms = NULL;
  }
  if (x->sourceinfo) {
    for (uint32_t i = 0; i < x->file_sources + x->live_sources; i++) {
      asdf_sourceinfo_free(x->sourceinfo[i]);
    }
    freebytes(
        x->sourceinfo,
        (x->file_sources + x->live_sources) * sizeof(AsdfSourceInfo *));
    x->sourceinfo = NULL;
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
  freebytes(x->outlet_ptrs, x->signal_outlets * sizeof(float *));
  /* NB: we don't free inlets/outlets because we hope Pd does it for us */
}

void update_buffers(t_asdf_tilde *x)
{
  for (uint32_t i = 0; i < x->file_sources; i++) {
    if (i < x->signal_outlets) {
      x->file_source_ptrs[i] = x->outlet_ptrs[i];
    } else {
      /* NB: A single channel is reused multiple times if necessary */
      x->file_source_ptrs[i] = x->dummy_source_data;
    }
  }
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

  char dirresult[MAXPDSTRING], *nameresult;
  int fd = canvas_open(x->x_canvas, s->s_name, "", dirresult, &nameresult,
      sizeof(dirresult), 0 /* open as binary */);
  if (fd < 0) {
    pd_error(&x->x_obj, "asdf~: Error opening \"%s\"", s->s_name);
    return;
  }
  /* we don't actually need the file descriptor: */
  sys_close(fd);

  char filename[MAXPDSTRING];
  snprintf(filename, sizeof(filename), "%s/%s", dirresult, nameresult);

  post("asdf~: opening ASDF scene \"%s\"", filename);
  /* TODO: reasonable default values? */
  /* TODO: ability to overwrite default values? */
  float sleep_time = 0.1;  /* seconds */
  float buffer_time = 2.0;  /* seconds */
  float samplerate = sys_getsr();
  uint32_t buffer_blocks = ceil(buffer_time * samplerate / (float)x->blocksize);
  uint64_t usleeptime = sleep_time * 1000.0f * 1000.0f;
  x->scene = asdf_scene_new(
      filename,
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
  x->live_sources = asdf_live_sources(x->scene);

  post("asdf~: opened scene with %u file source(s) and %u live source(s)",
      x->file_sources, x->live_sources);
  if (x->signal_outlets < x->file_sources) {
    /* NB: this is just a warning, available outlets will receive audio data */
    pd_error(
        &x->x_obj,
        "asdf~: fewer signal outlets (%u) than file sources (%u)",
        x->signal_outlets,
        x->file_sources);
  }

  if (x->file_sources > x->signal_outlets) {
    x->dummy_source_data = getbytes(x->blocksize * sizeof(float));
  }
  x->file_source_ptrs = getbytes(x->file_sources * sizeof(float *));
  x->transforms = getbytes(
      (1 + x->file_sources + x->live_sources) * sizeof(AsdfTransform));

  update_buffers(x);

  x->sourceinfo = getbytes(
      (x->file_sources + x->live_sources) * sizeof(AsdfSourceInfo *));
  for (uint32_t i = 0; i < x->file_sources + x->live_sources; i++) {
    x->sourceinfo[i] = asdf_get_sourceinfo(x->scene, i);
  }

  /* TODO: send number of sources to outlet */
  /* TODO: send id/name/model of sources to outlet */
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

void asdf_tilde_source_numbers(t_asdf_tilde *x, t_floatarg f)
{
  float arg = f;
  if (arg == 0.0) {
    x->send_source_ids = true;
  } else if (arg == 1.0) {
    x->send_source_ids = false;
  } else {
    pd_error(&x->x_obj,
        "asdf~: \"source-numbers\" should be followed by 0 or 1, not %f", arg);
  }
}

void send_transform(
    size_t offset,
    AsdfTransform *current,
    AsdfTransform *previous,
    t_outlet *outlet,
    t_atom *argv)
{
  t_symbol *sym;
  if (offset) {
    sym = gensym("src");
    if (current->active != previous->active) {
      SETSYMBOL(argv + offset, gensym("active"));
      SETFLOAT(argv + offset + 1, current->active);
      outlet_anything(outlet, sym, 3, argv);
    }
  } else {
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

    if (!previous->active
      || current->rot_v[0] != previous->rot_v[0]
      || current->rot_v[1] != previous->rot_v[1]
      || current->rot_v[2] != previous->rot_v[2]
      || current->rot_s != previous->rot_s)
    {
      SETSYMBOL(argv + offset, gensym("quat"));
      SETFLOAT(argv + offset + 1, current->rot_v[0]);
      SETFLOAT(argv + offset + 2, current->rot_v[1]);
      SETFLOAT(argv + offset + 3, current->rot_v[2]);
      SETFLOAT(argv + offset + 4, current->rot_s);
      outlet_anything(outlet, sym, offset + 5, argv);
      previous->rot_v[0] = current->rot_v[0];
      previous->rot_v[1] = current->rot_v[1];
      previous->rot_v[2] = current->rot_v[2];
      previous->rot_s = current->rot_s;
    }

    if (!previous->active
      || current->vol != previous->vol)
    {
      SETSYMBOL(argv + offset, gensym("vol"));
      SETFLOAT(argv + offset + 1, current->vol);
      outlet_anything(outlet, sym, offset + 2, argv);
      previous->vol = current->vol;
    }
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
    if (x->send_source_ids) {
      SETSYMBOL(argv, gensym(x->sourceinfo[idx]->id));
    } else {
      /* NB: one-based source numbers per SSR convention */
      SETFLOAT(argv, idx + 1);
    }
    current = asdf_get_source_transform(x->scene, idx, x->frame);
    AsdfTransform *previous = &x->transforms[idx + 1];
    send_transform(1, &current, previous, x->x_obj.ob_outlet, argv);
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
    switch (asdf_get_audio_data(x->scene, x->file_source_ptrs, rolling)) {
      case ASDF_STREAMING_SUCCESS:
        if (rolling) {
          x->frame += blocksize;
        }
        break;
      case ASDF_STREAMING_EMPTY_BUFFER:
        pd_error(&x->x_obj, "asdf~: empty file streaming buffer");
        break;
      case ASDF_STREAMING_INCOMPLETE_SEEK:
        pd_error(&x->x_obj, "asdf~: BUG: incomplete seek");
        break;
      case ASDF_STREAMING_SEEK_WHILE_ROLLING:
        pd_error(&x->x_obj, "asdf~: BUG: seek while rolling");
        break;
    }
  }
  /* fill unused outlets with zeros */
  for (uint32_t i = x->file_sources; i < x->signal_outlets; i++) {
    t_sample *target = x->outlet_ptrs[i];
    for (uint32_t j = 0; j < blocksize; j++) {
      /* TODO: use memset()? */
      target[j] = 0.0;
    }
  }
  return w + 2;
}

void asdf_tilde_dsp(t_asdf_tilde *x, t_signal **sp)
{
  if (x->signal_outlets && x->blocksize != (uint32_t)sp[0]->s_n) {
    /* TODO: is there a way to avoid this limitation? */
    pd_error(
        &x->x_obj,
        "asdf~ must run with the system block size %u, not %i",
        x->blocksize,
        sp[0]->s_n);
    return;
  }
  for (uint32_t i = 0; i < x->signal_outlets; i++) {
    x->outlet_ptrs[i] = sp[i]->s_vec;
  }
  update_buffers(x);
  dsp_add(asdf_tilde_perform, 1, (t_int)x);
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

  class_addmethod(asdf_tilde_class,
      (t_method)asdf_tilde_source_numbers, gensym("source-numbers"),
      A_DEFFLOAT, 0);

  class_addbang(asdf_tilde_class, asdf_tilde_bang);

  class_addmethod(asdf_tilde_class,
      (t_method)asdf_tilde_dsp, gensym("dsp"), A_CANT, 0);
}
