/*
 * gzlib.c - interface to the 'zlib' compression library
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <zlib.h>
#include "networking-headers.h"
#include "unused.h"
#include "libserveez/alloc.h"
#include "libserveez/codec/codec.h"
#include "libserveez/codec/gzlib.h"

/* Definition of the 'zlib' encoder.  */
svz_codec_t zlib_encoder = {
  "zlib",
  SVZ_CODEC_ENCODER,
  zlib_encoder_init,
  zlib_encoder_finalize,
  zlib_encode,
  zlib_error,
  zlib_ratio,
  NULL,
  0
};

/* Definition of the 'zlib' decoder.  */
svz_codec_t zlib_decoder = {
  "zlib",
  SVZ_CODEC_DECODER,
  zlib_decoder_init,
  zlib_decoder_finalize,
  zlib_decode,
  zlib_error,
  zlib_ratio,
  "\x78\x9c",
  2
};

/* Internal 'zlib' data structure.  The arbitrary `data' field of the
   @code{svz_codec_data_t} structure is used to hold this data structure
   and is thus passed to each codec callback.  */
typedef struct zlib_data
{
  z_stream stream; /* 'z_stream' is representation of a 'zlib' stream */
}
zlib_data_t;

/* Allocator functions passed to the zlib in order to allocate and free
   memory.  This is useful when using a custom memory management.  Because
   we assign here the Serveez core library's allocators it is possible to
   detect memory leaks within calls to the 'zlib' library.  */
static voidpf
zlib_alloc (UNUSED voidpf data, uInt n, uInt size)
{
  voidpf ptr;
  if ((ptr = (voidpf) svz_malloc (n * size)) == NULL)
    return Z_NULL;
  return ptr;
}

static void
zlib_free (UNUSED voidpf data, voidpf ptr)
{
  svz_free (ptr);
}

/* Codec `error' callback:
   Returns the last 'zlib' codec error.  This callback gets invoked in order
   to obtain a human readable error message if any of the codec callbacks
   returns an error.  */
char *
zlib_error (svz_codec_data_t *data)
{
  zlib_data_t *z;
  static char err[256];

  strcpy (err, "Codec released");
  if (data)
    {
      z = data->data;
      sprintf (err, "%s", z ? z->stream.msg : "No error");
    }
  return err;
}

/* Codec `ratio' callback:
   Returns the current ratio state of 'zlib' codec.  This callback gets
   called if the `code' callback returned @code{SVZ_CODEC_FINISHED}.  */
int
zlib_ratio (svz_codec_data_t *data, size_t *in, size_t *out)
{
  zlib_data_t *z;

  if (data)
    {
      z = data->data;
      *in = (size_t) z->stream.total_in;
      *out = (size_t) z->stream.total_out;
      return SVZ_CODEC_OK;
    }
  return SVZ_CODEC_ERROR;
}

/* Codec `init' callback:
   Initialization routine for the 'zlib' encoder.  This callback is run when
   the codec is setup for sending or receiving.  It should return
   @code{SVZ_CODEC_ERROR} on failure and @code{SVZ_CODEC_OK} otherwise.  */
int
zlib_encoder_init (svz_codec_data_t *data)
{
  zlib_data_t *z;

  z = (zlib_data_t *) zlib_alloc (NULL, 1, sizeof (zlib_data_t));
  memset (z, 0, sizeof (zlib_data_t));
  data->data = (void *) z;
  z->stream.zalloc = zlib_alloc;
  z->stream.zfree = zlib_free;
  z->stream.opaque = Z_NULL;

  if (deflateInit (&z->stream, Z_DEFAULT_COMPRESSION) != Z_OK)
    return SVZ_CODEC_ERROR;
  return SVZ_CODEC_OK;
}

/* Codec `finalize' callback:
   Finalizer routine for the 'zlib' encoder.  This callback is called by
   Serveez's codec interface if encoding has ended and should revert the
   setup done in the `init' callback.  */
int
zlib_encoder_finalize (svz_codec_data_t *data)
{
  zlib_data_t *z = (zlib_data_t *) data->data;
  int ret = SVZ_CODEC_OK;

  if (z != NULL)
    {
      if (deflateEnd (&z->stream) != Z_OK)
        ret = SVZ_CODEC_ERROR;
      zlib_free (NULL, (voidpf) z);
      data->data = NULL;
    }
  return ret;
}

/* Codec `code' callback:
   Encoding routine of 'zlib' codec.  The callback is meant to do what is
   described by the `flag' member of @code{svz_codec_data_t}.  The coding
   routine must interpret the input buffer and output buffer description
   correctly and should remove the input bytes consumed by the codec.
   Possible return values are @code{SVZ_CODEC_ERROR}, @code{SVZ_CODEC_OK},
   @code{SVZ_CODEC_MORE_OUT} and @code{SVZ_CODEC_MORE_IN} each having its
   special meaning.  */
int
zlib_encode (svz_codec_data_t *data)
{
  zlib_data_t *z = (zlib_data_t *) data->data;
  z_stream *s = &z->stream;
  int flush = 0, ret;

  s->next_in = (Bytef *) data->in_buffer;
  s->avail_in = (uInt) data->in_fill;
  s->next_out = (Bytef *) data->out_buffer + data->out_fill;
  s->avail_out = (uInt) data->out_size - data->out_fill;

  if (data->flag & SVZ_CODEC_FLUSH)
    flush = Z_SYNC_FLUSH;

  if (data->flag & SVZ_CODEC_FINISH)
    flush = Z_FINISH;

  ret = deflate (s, flush);
  if (ret != Z_OK && ret != Z_STREAM_END)
    return SVZ_CODEC_ERROR;

  /* Correct the values in the input and output buffer.  */
  if (s->avail_in > 0)
    memmove (data->in_buffer, s->next_in, s->avail_in);
  data->in_fill = s->avail_in;
  data->out_fill = (int) data->out_size - s->avail_out;

  if (s->avail_out == 0)
    return SVZ_CODEC_MORE_OUT;

  return ret == Z_STREAM_END ? SVZ_CODEC_FINISHED : SVZ_CODEC_OK;
}

/* Initialization routine for the 'zlib' decoder.  */
int
zlib_decoder_init (svz_codec_data_t *data)
{
  zlib_data_t *z;

  z = (zlib_data_t *) zlib_alloc (NULL, 1, sizeof (zlib_data_t));
  memset (z, 0, sizeof (zlib_data_t));
  data->data = (void *) z;
  z->stream.zalloc = zlib_alloc;
  z->stream.zfree = zlib_free;
  z->stream.opaque = Z_NULL;

  if (inflateInit (&z->stream) != Z_OK)
    return SVZ_CODEC_ERROR;
  return SVZ_CODEC_OK;
}

/* Finalizer routine for the 'zlib' decoder.  */
int
zlib_decoder_finalize (svz_codec_data_t *data)
{
  zlib_data_t *z = (zlib_data_t *) data->data;
  int ret = SVZ_CODEC_OK;

  if (z != NULL)
    {
      if (inflateEnd (&z->stream) != Z_OK)
        ret = SVZ_CODEC_ERROR;
      zlib_free (NULL, (voidpf) z);
      data->data = NULL;
    }
  return ret;
}

/* Decoding routine of the 'zlib' codec.  */
int
zlib_decode (svz_codec_data_t *data)
{
  zlib_data_t *z = (zlib_data_t *) data->data;
  z_stream *s = &z->stream;
  int flush = 0, ret;

  s->next_in = (Bytef *) data->in_buffer;
  s->avail_in = (uInt) data->in_fill;
  s->next_out = (Bytef *) data->out_buffer + data->out_fill;
  s->avail_out = (uInt) data->out_size - data->out_fill;

  if (data->flag & SVZ_CODEC_FLUSH)
    flush = Z_SYNC_FLUSH;

  if (data->flag & SVZ_CODEC_FINISH)
    flush = Z_FINISH;

  ret = inflate (s, flush);
  if (ret != Z_OK && ret != Z_STREAM_END && ret != Z_BUF_ERROR)
    return SVZ_CODEC_ERROR;

  if (s->avail_in > 0)
    memmove (data->in_buffer, s->next_in, s->avail_in);
  data->in_fill = s->avail_in;
  data->out_fill = (int) data->out_size - s->avail_out;

  if (s->avail_out == 0 && ret != Z_STREAM_END)
    return SVZ_CODEC_MORE_OUT;

  return ret == Z_STREAM_END ? SVZ_CODEC_FINISHED : SVZ_CODEC_OK;
}
