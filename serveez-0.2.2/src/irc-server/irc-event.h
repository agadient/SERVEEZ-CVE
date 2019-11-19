/*
 * irc-event.h - IRC event header definitions
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000 Stefan Jahn <stefan@lkcc.org>
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

#ifndef __IRC_EVENT_H__
#define __IRC_EVENT_H__

/*************************************************************************
 * This is a list of IRC Messages i need to reply on.  Every section of
 * this table is implemented in a file called "irc-event-?.c" where the
 * ? refers to the subsection's # seen here in the table.
 * All the callbacks get three (3) args passed these are in specific:
 * 1. the socket structure for the IRC connection,
 * 2. the IRC client structure for this socket and
 * 3. the already parsed request.
 *
 *************************************************************************
 *
 * Message                             * Done * Status
 *************************************************************************
 * 4.1 Connection Registration
 *    4.1.1 Password message           * Yes  * Ok -> but Modified
 *    4.1.2 Nickname message           * Yes  * Ok
 *    4.1.3 User message               * Yes  * Ok
 *    4.1.4 Server message             * No   *
 *    4.1.5 Operator message           * Yes  * Seems Ok
 *    4.1.6 Quit message               * Yes  * Ok
 *    4.1.7 Server Quit message        * No   *
 * 4.2 Channel operations
 *    4.2.1 Join message               * Yes  * Ok
 *    4.2.2 Part message               * Yes  * Ok
 *    4.2.3 Mode message               * Yes  * ToDo, Much
 *       4.2.3.1 Channel modes         * Yes  * ToDo
 *       4.2.3.2 User modes            * Yes  * ToDo
 *    4.2.4 Topic message              * Yes  * Ok (incl. extra)
 *    4.2.5 Names message              * Yes  * Ok
 *    4.2.6 List message               * Yes  * Ok
 *    4.2.7 Invite message             * Yes  * Ok
 *    4.2.8 Kick message               * Yes  * Ok
 * 4.3 Server queries and commands
 *    4.3.1 Version message            * Yes  * Ok
 *    4.3.2 Stats message              * Yes  * just started, Much (!) todo
 *    4.3.3 Links message              * No   *
 *    4.3.4 Time message               * Yes  * Ok
 *    4.3.5 Connect message            * No   *
 *    4.3.6 Trace message              * No   *
 *    4.3.7 Admin message              * Yes  *
 *    4.3.8 Info message               * Yes  *
 * 4.4 Sending messages
 *    4.4.1 Private messages           * Yes  * Ok
 *    4.4.2 Notice messages            * Yes  * Ok
 * 4.5 User-based queries
 *    4.5.1 Who query                  * Yes  * not all done (o)
 *    4.5.2 Whois query                * Yes  * Ok
 *    4.5.3 Whowas message             * Yes  *
 * 4.6 Miscellaneous messages
 *    4.6.1 Kill message               * No   * Partly ???
 *    4.6.2 Ping message               * Yes  *
 *    4.6.3 Pong message               * Yes  *
 *    4.6.4 Error message              * Yes  * Much ToDo
 * 5. OPTIONAL MESSAGES
 *    5.1 Away message                 * Yes  *
 *    5.2 Rehash command               * No   *
 *    5.3 Restart command              * No   *
 *    5.4 Summon message               * No   *
 *    5.5 Users message                * Yes  *
 *    5.6 Operwall command             * No   *
 *    5.7 Userhost message             * Yes  * Ok
 *    5.8 Ison message                 * Yes  * Ok
 *
 *************************************************************************/

/* Miscellaneous functions */
char *irc_client_flag_string (irc_client_t *client);
char *irc_channel_flag_string (irc_channel_t *channel);
int irc_register_client (svz_socket_t *, irc_client_t *, irc_config_t *);
void irc_destroy_ban (irc_ban_t *ban);

#define CALLBACK_ARGS svz_socket_t *, irc_client_t *, irc_request_t *

/* Connection Registration */
int irc_pass_callback (CALLBACK_ARGS);
int irc_user_callback (CALLBACK_ARGS);
int irc_nick_callback (CALLBACK_ARGS);
int irc_quit_callback (CALLBACK_ARGS);
int irc_motd_callback (CALLBACK_ARGS);
int irc_oper_callback (CALLBACK_ARGS);

/* Channel operations */
int irc_part_callback   (CALLBACK_ARGS);
int irc_join_callback   (CALLBACK_ARGS);
int irc_mode_callback   (CALLBACK_ARGS);
int irc_topic_callback  (CALLBACK_ARGS);
int irc_names_callback  (CALLBACK_ARGS);
int irc_list_callback   (CALLBACK_ARGS);
int irc_invite_callback (CALLBACK_ARGS);
int irc_kick_callback   (CALLBACK_ARGS);

/* Server queries and commands */
int irc_stats_callback   (CALLBACK_ARGS);
int irc_version_callback (CALLBACK_ARGS);
int irc_lusers_callback  (CALLBACK_ARGS);
int irc_time_callback    (CALLBACK_ARGS);
int irc_admin_callback   (CALLBACK_ARGS);
int irc_info_callback    (CALLBACK_ARGS);

/* Sending messages */
int irc_note_callback (CALLBACK_ARGS);
int irc_priv_callback (CALLBACK_ARGS);

/* User-based queries */
int irc_who_callback    (CALLBACK_ARGS);
int irc_whois_callback  (CALLBACK_ARGS);
int irc_whowas_callback (CALLBACK_ARGS);

/* Miscellaneous messages */
int irc_ping_callback  (CALLBACK_ARGS);
int irc_pong_callback  (CALLBACK_ARGS);
int irc_error_callback (CALLBACK_ARGS);
int irc_kill_callback  (CALLBACK_ARGS);

/* Optional messages */
int irc_away_callback     (CALLBACK_ARGS);
int irc_userhost_callback (CALLBACK_ARGS);
int irc_ison_callback     (CALLBACK_ARGS);
int irc_users_callback    (CALLBACK_ARGS);

#endif /* __IRC_EVENT_H__ */
