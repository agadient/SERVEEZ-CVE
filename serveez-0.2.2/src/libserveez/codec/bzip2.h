/*
 * bzip2.h - interface to the 'bzip2' block-sorting compression library
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

#ifndef __BZIP2_H__
#define __BZIP2_H__ 1

#include "libserveez/defines.h"

/* Configuration structure for the bzip2 codec.  */
typedef struct
{
  int blockSize100k; /* block size in 100 KByte */
  int verbosity;     /* verbosity */
  int workFactor;    /* workFactor */
  int small;         /* use an alternative decompression algorithm */
}
bzip2_config_t;

__BEGIN_DECLS
SBO svz_codec_t bzip2_encoder;
SBO svz_codec_t bzip2_decoder;
SBO char * bzip2_error (svz_codec_data_t *);
SBO int bzip2_encoder_init (svz_codec_data_t *);
SBO int bzip2_encoder_finalize (svz_codec_data_t *);
SBO int bzip2_encode (svz_codec_data_t *);
SBO int bzip2_decoder_init (svz_codec_data_t *);
SBO int bzip2_decoder_finalize (svz_codec_data_t *);
SBO int bzip2_decode (svz_codec_data_t *);
SBO int bzip2_ratio (svz_codec_data_t *, size_t *, size_t *);

__END_DECLS

#endif /* not __BZIP2_H__ */
