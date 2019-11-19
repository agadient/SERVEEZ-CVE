/*
 * bzip2.c - interface to the 'bzip2' block-sorting compression library
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
#include <bzlib.h>
#include "networking-headers.h"
#include "unused.h"
#include "libserveez/alloc.h"
#include "libserveez/codec/codec.h"
#include "libserveez/codec/bzip2.h"

/* Version 1.0 and above use the `BZ2_' prefix to avoid namespace
   pollution.  */
#if HAVE_BZ2LIB_PREFIX
# define bzCompressInit   BZ2_bzCompressInit
# define bzCompress       BZ2_bzCompress
# define bzCompressEnd    BZ2_bzCompressEnd
# define bzDecompressInit BZ2_bzDecompressInit
# define bzDecompress     BZ2_bzDecompress
# define bzDecompressEnd  BZ2_bzDecompressEnd
# define bzlibVersion     BZ2_bzlibVersion
# define bzerror          BZ2_bzerror
#endif /* HAVE_BZ2LIB_PREFIX */

/* Definition of the 'bzip2' encoder.  */
svz_codec_t bzip2_encoder = {
  "bzip2",
  SVZ_CODEC_ENCODER,
  bzip2_encoder_init,
  bzip2_encoder_finalize,
  bzip2_encode,
  bzip2_error,
  bzip2_ratio,
  NULL,
  0
};

/* Definition of the 'bzip2' decoder.  */
svz_codec_t bzip2_decoder = {
  "bzip2",
  SVZ_CODEC_DECODER,
  bzip2_decoder_init,
  bzip2_decoder_finalize,
  bzip2_decode,
  bzip2_error,
  bzip2_ratio,
  "BZh",
  3
};

/* Default configuration.  */
static bzip2_config_t bzip2_config = {
  9, /* block size in 100 KByte */
  0, /* verbosity */
  0, /* workFactor */
  0  /* use an alternative decompression algorithm */
};

/* Internal codec data.  Passed to each call of the codec callbacks in the
   `data' field of @code{svz_codec_data_t}.  */
typedef struct bzip2_data
{
  bz_stream stream; /* 'bz_stream' is representation of a 'bzip2' stream */
  int error;        /* last error */
}
bzip2_data_t;

/* Customized allocator functions.  */
static void *
bzip2_alloc (UNUSED void *opaque, int n, int size)
{
  void *ptr;

  if ((ptr = (void *) svz_malloc (n * size)) != NULL)
    return ptr;
  return NULL;
}

static void
bzip2_free (UNUSED void *opaque, void *ptr)
{
  svz_free (ptr);
}

/* Returns the text representation of the last error associated with
   the current 'bzip2' stream.  */
char *
bzip2_error (svz_codec_data_t *data)
{
  bzip2_data_t *bz;
  static char err[256];

  strcpy (err, "Codec released");
  if (data == NULL)
    return err;
  if ((bz = data->data) == NULL)
    return err;

  switch (bz->error)
    {
    case BZ_OK:
    case BZ_RUN_OK:
    case BZ_FLUSH_OK:
    case BZ_FINISH_OK:
      strcpy (err, "Operation completed successfully");
      break;
    case BZ_STREAM_END:
      strcpy (err, "Compression of data completed");
      break;
#ifdef BZ_CONFIG_ERROR
    case BZ_CONFIG_ERROR:
      strcpy (err, "Major configuration error");
      break;
#endif
    case BZ_SEQUENCE_ERROR:
      strcpy (err, "Sequence error");
      break;
    case BZ_PARAM_ERROR:
      strcpy (err, "Invalid argument");
      break;
    case BZ_MEM_ERROR:
      strcpy (err, "Out of memory");
      break;
    case BZ_DATA_ERROR:
      strcpy (err, "Data integrity error detected");
      break;
    case BZ_DATA_ERROR_MAGIC:
      strcpy (err, "Invalid magic code");
      break;
    case BZ_IO_ERROR:
      sprintf (err, "I/O error: %s", strerror (errno));
      break;
    case BZ_UNEXPECTED_EOF:
      strcpy (err, "Unexpected EOF");
      break;
    case BZ_OUTBUFF_FULL:
      strcpy (err, "Output buffer full");
      break;
    default:
      strcpy (err, "No error");
    }
  return err;
}

/* Saves the number of total input and output bytes within @var{in} and
   @var{out}.  */
int
bzip2_ratio (svz_codec_data_t *data, size_t *in, size_t *out)
{
  bzip2_data_t *bz;

  if (data)
    {
      bz = data->data;
#if HAVE_BZ2LIB_PREFIX
# if SIZEOF_LONG <= 4
      *in = (size_t) bz->stream.total_in_lo32;
      *out = (size_t) bz->stream.total_out_lo32;
# else
      *in = (size_t) bz->stream.total_in_hi32 << 32;
      *in += (size_t) bz->stream.total_in_lo32;
      *out = (size_t) bz->stream.total_out_hi32 << 32;
      *out += (size_t) bz->stream.total_out_lo32;
# endif
#else
      *in = (size_t) bz->stream.total_in;
      *out = (size_t) bz->stream.total_out;
#endif
      return SVZ_CODEC_OK;
    }
  return SVZ_CODEC_ERROR;
}

/* Initialize the 'bzip2' codec for encoding (compression).  */
int
bzip2_encoder_init (svz_codec_data_t *data)
{
  bzip2_data_t *bz;
  bzip2_config_t *cfg;

  bz = bzip2_alloc (NULL, 1, sizeof (bzip2_data_t));
  memset (bz, 0, sizeof (bzip2_data_t));
  data->data = (void *) bz;
  data->config = cfg = &bzip2_config;
  bz->stream.bzalloc = bzip2_alloc;
  bz->stream.bzfree = bzip2_free;
  bz->stream.opaque = NULL;

  bz->error = bzCompressInit (&bz->stream, cfg->blockSize100k,
                              cfg->verbosity, cfg->workFactor);
  return (bz->error != BZ_OK) ? SVZ_CODEC_ERROR : SVZ_CODEC_OK;
}

/* Finalizes the 'bzip2' compressor.  */
int
bzip2_encoder_finalize (svz_codec_data_t *data)
{
  bzip2_data_t *bz = (bzip2_data_t *) data->data;
  int ret = SVZ_CODEC_OK;

  if (bz != NULL)
    {
      if ((bz->error = bzCompressEnd (&bz->stream)) != BZ_OK)
        ret = SVZ_CODEC_ERROR;
      bzip2_free (NULL, bz);
      data->data = data->config = NULL;
    }
  return ret;
}

/* Compression routine of the 'bzip2' codec.  Depending on the `flag' of the
   @code{svz_codec_data_t} structure @var{data} it just compresses more data,
   flushs the output buffer or finishs the output stream.  Returns special
   values to indicate buffer overruns and end of streams.  */
int
bzip2_encode (svz_codec_data_t *data)
{
  bzip2_data_t *bz = (bzip2_data_t *) data->data;
  bz_stream *s = &bz->stream;
  int action = BZ_RUN, ret;

  /* Adjust input and output buffers.  */
  s->next_in = (char *) data->in_buffer;
  s->avail_in = (unsigned int) data->in_fill;
  s->next_out = (char *) data->out_buffer + data->out_fill;
  s->avail_out = (unsigned int) data->out_size - data->out_fill;

  /* Check for additional flags.  */
  if (data->flag & SVZ_CODEC_FLUSH)
    action = BZ_FLUSH;

  if (data->flag & SVZ_CODEC_FINISH)
    action = BZ_FINISH;

  ret = bz->error = bzCompress (s, action);
  if (ret != BZ_RUN_OK && ret != BZ_FLUSH_OK && ret != BZ_FINISH_OK &&
      ret != BZ_STREAM_END)
    return SVZ_CODEC_ERROR;

  /* Correct the values in the input and output buffer.  */
  if (s->avail_in > 0)
    memmove (data->in_buffer, s->next_in, s->avail_in);
  data->in_fill = s->avail_in;
  data->out_fill = (int) data->out_size - s->avail_out;

  if (s->avail_out == 0)
    return SVZ_CODEC_MORE_OUT;

  return ret == BZ_STREAM_END ? SVZ_CODEC_FINISHED : SVZ_CODEC_OK;
}

/* Initialize the 'bzip2' decompressor.  */
int
bzip2_decoder_init (svz_codec_data_t *data)
{
  bzip2_data_t *bz;
  bzip2_config_t *cfg;

  bz = bzip2_alloc (NULL, 1, sizeof (bzip2_data_t));
  memset (bz, 0, sizeof (bzip2_data_t));
  data->data = (void *) bz;
  data->config = cfg = &bzip2_config;
  bz->stream.bzalloc = bzip2_alloc;
  bz->stream.bzfree = bzip2_free;
  bz->stream.opaque = NULL;

  bz->error = bzDecompressInit (&bz->stream, cfg->verbosity, cfg->small);
  return (bz->error != BZ_OK) ? SVZ_CODEC_ERROR : SVZ_CODEC_OK;
}

/* Finalize the 'bzip2' decompressor.  */
int
bzip2_decoder_finalize (svz_codec_data_t *data)
{
  bzip2_data_t *bz = (bzip2_data_t *) data->data;
  int ret = SVZ_CODEC_OK;

  if (bz != NULL)
    {
      if ((bz->error = bzDecompressEnd (&bz->stream)) != BZ_OK)
        ret = SVZ_CODEC_ERROR;
      bzip2_free (NULL, bz);
      data->data = data->config = NULL;
    }
  return ret;
}

/* Decompresses data depending on the `flag' member of @var{data}.  Returns
   indicators about what happens next.  */
int
bzip2_decode (svz_codec_data_t *data)
{
  bzip2_data_t *bz = (bzip2_data_t *) data->data;
  bz_stream *s = &bz->stream;
  int ret;

  /* Adjust input and output buffers.  */
  s->next_in = (char *) data->in_buffer;
  s->avail_in = (unsigned int) data->in_fill;
  s->next_out = (char *) data->out_buffer + data->out_fill;
  s->avail_out = (unsigned int) data->out_size - data->out_fill;

  ret = bz->error = bzDecompress (s);
  if (ret != BZ_OK && ret != BZ_STREAM_END)
    return SVZ_CODEC_ERROR;

  if (s->avail_in > 0)
    memmove (data->in_buffer, s->next_in, s->avail_in);
  data->in_fill = s->avail_in;
  data->out_fill = (int) data->out_size - s->avail_out;

  if (s->avail_out == 0 && ret != BZ_STREAM_END)
    return SVZ_CODEC_MORE_OUT;

  return ret == BZ_STREAM_END ? SVZ_CODEC_FINISHED : SVZ_CODEC_OK;
}
