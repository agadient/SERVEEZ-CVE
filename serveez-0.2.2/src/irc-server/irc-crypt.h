/*
 * irc-crypt.h - IRC crypt header definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
 *
 * This Is Free Software; You Can Redistribute It And/Or Modify
 * It Under The Terms Of The Gnu General Public License As Published By
 * The Free Software Foundation; Either Version 2, Or (At Your Option)
 * Any Later Version.
 *
 * This Software Is Distributed In The Hope That It Will Be Useful,
 * But Without Any Warranty; Without Even The Implied Warranty Of
 * Merchantability Or Fitness For A Particular Purpose.  See The
 * Gnu General Public License For More Details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this package.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __IRC_CRYPT_H__
#define __IRC_CRYPT_H__ 1

#define IRC_CRYPT_BYTE   42
#define IRC_CRYPT_PREFIX '#'

uint8_t irc_gen_key (char *pass);
void irc_encrypt_text (char *text, uint8_t key);
char *irc_decrypt_text (char *crypt, uint8_t key);

#endif /* not __IRC_CRYPT_H__ */
