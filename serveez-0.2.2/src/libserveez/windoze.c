/*
 * windoze.c - windows port implementations
 *
 * Copyright (C) 2011-2013 Thien-Thi Nguyen
 * Copyright (C) 2000, 2001 Stefan Jahn <stefan@lkcc.org>
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

#include <shellapi.h>
#include <windowsx.h>

#include "networking-headers.h"
#include "woe-wait.h"
#include "libserveez/boot.h"
#include "libserveez/util.h"
#include "libserveez/socket.h"
#include "libserveez/server-core.h"
#include "libserveez/windoze.h"

/* window definitions */
#define WM_SERVEEZ_NOTIFYICON (WM_APP + 100)
#define SERVEEZ_ICON_ID       (1001)
#define SERVEEZ_CLASS         "serveez"

static DWORD windoze_daemon_id = 0;
static HANDLE windoze_daemon_handle = NULL;
static BOOL windoze_run = FALSE;
static HICON windoze_icon = NULL;
static char windoze_tooltip[128];

/*
 * Modify the windows taskbar.
 */
static BOOL
windoze_set_taskbar (HWND hwnd, DWORD msg, UINT id, HICON icon, PSTR tip)
{
  NOTIFYICONDATA tnd;

  /* setup taskbar icon */
  tnd.cbSize = sizeof (NOTIFYICONDATA);
  tnd.hWnd = hwnd;
  tnd.uID = id;
  tnd.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP;
  tnd.uCallbackMessage = WM_SERVEEZ_NOTIFYICON;
  tnd.hIcon = icon;

  if (tip)
    {
      strncpy (tnd.szTip, tip, sizeof (tnd.szTip));
    }
  else
    {
      tnd.szTip[0] = '\0';
    }

  return Shell_NotifyIcon (msg, &tnd);;
}

/*
 * Modify what's within the taskbar.
 */
static void
windoze_notify_set (HWND hwnd, UINT id)
{
  sprintf (windoze_tooltip, "%s (%d connections)",
           PACKAGE_STRING, svz_sock_connections);

  windoze_set_taskbar (hwnd, NIM_MODIFY, id, windoze_icon, windoze_tooltip);
}

/*
 * Delete something from the taskbar.
 */
static void
windoze_notify_del (HWND hwnd, UINT id)
{
  windoze_set_taskbar (hwnd, NIM_DELETE, id, NULL, NULL);
}

/*
 * Add something to the taskbar.
 */
static void
windoze_notify_add (HWND hwnd, UINT id)
{
  sprintf (windoze_tooltip, "%s", PACKAGE_STRING);

  windoze_set_taskbar (hwnd, NIM_ADD, id, windoze_icon, windoze_tooltip);
}

/*
 * Draw the serveez icon within the taskbar.
 */
static LRESULT
windoze_draw_icon (LPDRAWITEMSTRUCT lpdi)
{
  DrawIconEx (lpdi->hDC, lpdi->rcItem.left, lpdi->rcItem.top, windoze_icon,
              16, 16, 0, NULL, DI_NORMAL);

  return TRUE;
}

/*
 * Dialog callback procedure.
 */
static LRESULT CALLBACK
windoze_dialog (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
  switch (msg)
    {
    case WM_DRAWITEM:
      return (windoze_draw_icon ((LPDRAWITEMSTRUCT) lparam));
      break;

    case WM_DESTROY:
      windoze_notify_del (hwnd, SERVEEZ_ICON_ID);
      svz_nuke_happened = 1;
      break;

    case WM_COMMAND:
      break;

    case WM_SERVEEZ_NOTIFYICON:
      switch (lparam)
        {
        case WM_LBUTTONDOWN:
          break;

        case WM_RBUTTONDOWN:
          svz_nuke_happened = 1;
          break;

        default:
          break;
        }
      break;

    default:
      break;
    }

  return DefWindowProc (hwnd, msg, wparam, lparam);
}

/*
 * Main window thread where the window manager can pass messages to.
 */
static DWORD WINAPI
windoze_thread (char *prog)
{
  HWND hwnd;      /* window handle */
  MSG msg;        /* message structure */
  WNDCLASS class; /* window class */
  ATOM atom;      /* window manager atom */
  int count = 0;  /* notify interval counter */

  /* load appropriate icon */
  windoze_icon = LoadIcon (GetModuleHandle (prog), "SERVEEZ_ICON_TINY");
  if (windoze_icon == NULL)
    {
      svz_log_sys_error ("LoadIcon");
      windoze_icon = LoadIcon (NULL, IDI_WINLOGO);
    }

  /* setup window class */
  memset (&class, 0, sizeof (class));
  class.lpfnWndProc = windoze_dialog;
  class.hInstance = GetModuleHandle (prog);
  class.lpszClassName = SERVEEZ_CLASS;

  /* register window class */
  if ((atom = RegisterClass (&class)) == 0)
    {
      svz_log_sys_error ("RegisterClass");
      ExitThread (0);
    }

  /* create new main window */
  hwnd = CreateWindow (SERVEEZ_CLASS, NULL, 0, 0, 0, 0, 0,
                       NULL, NULL, GetModuleHandle (prog), NULL);
  if (hwnd == NULL)
    {
      svz_log_sys_error ("CreateWindow");
      ExitThread (0);
    }

  /* add icon to task bar */
  windoze_notify_add (hwnd, SERVEEZ_ICON_ID);

  /* start message loop */
  windoze_run = TRUE;
  while (windoze_run)
    {
      if (PeekMessage (&msg, hwnd, 0, 0, PM_REMOVE))
        {
          windoze_dialog (msg.hwnd, msg.message, msg.wParam, msg.lParam);
        }
      Sleep (50);

      /* modify tooltip regularly */
      if ((count += 50) >= 1000)
        {
          windoze_notify_set (hwnd, SERVEEZ_ICON_ID);
          count = 0;
        }
    }
  windoze_run = FALSE;

  /* delete icon from taskbar */
  windoze_notify_del (hwnd, SERVEEZ_ICON_ID);

  /* destroy window and window class */
  if (!DestroyWindow (hwnd))
    {
      svz_log_sys_error ("DestroyWindow");
    }
  if (!UnregisterClass (SERVEEZ_CLASS, GetModuleHandle (prog)))
    {
      svz_log_sys_error ("UnRegisterClass");
    }

  return 0;
}

/*
 * Start the windows daemon thread and detach from the current console.
 */
static int
start_daemon (char *prog)
{
  /* start message loop */
  if ((windoze_daemon_handle = CreateThread (
        NULL,
        0,
        (LPTHREAD_START_ROUTINE) windoze_thread,
        prog,
        0,
        &windoze_daemon_id)) == NULL)
    {
      svz_log_sys_error ("CreateThread");
      return -1;
    }

  /* detach program from console */
  if (!FreeConsole ())
    {
      svz_log_sys_error ("FreeConsole");
      return -1;
    }

  return 0;
}

/*
 * Stop the windows daemon thread.
 */
static int
stop_daemon (void)
{
  DWORD ret;

  /* daemon thread still running?  */
  if (!windoze_run)
    {
      return 0;
    }

  /* signal termination */
  windoze_run = FALSE;
  PostQuitMessage (0);

  /* wait until daemon thread terminated */
  ret = WOE_WAIT_INF (windoze_daemon_handle);
  if (ret != WAIT_OBJECT_0)
    {
      WOE_WAIT_LOG_ERROR_ANONYMOUSLY ();
      return -1;
    }

  /* close thread handle */
  if (!CloseHandle (windoze_daemon_handle))
    {
      svz_log_sys_error ("CloseHandle");
      return -1;
    }
  return 0;
}

/**
 * If @var{prog} is non-NULL, start the daemon thread with it.
 * Otherwise (if @var{prog} is NULL), stop the daemon thread.
 * Return 0 on success, -1 on failure.
 */
int
svz_windoze_daemon_control (char *prog)
{
  return prog
    ? start_daemon (prog)
    : stop_daemon ();
}

/*
 * Read an unsigned integer value from the Windows Registry Database.
 */
unsigned
svz_windoze_get_reg_unsigned (HKEY key, char *subkey,
                              char *subsubkey, unsigned def)
{
  unsigned value;
  DWORD size, type;
  HKEY reg;

  if (RegOpenKeyEx (key, subkey, 0, KEY_QUERY_VALUE, &reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegOpenKeyEx");
      return def;
    }

  size = sizeof (DWORD);
  type = REG_DWORD;
  if (RegQueryValueEx (reg, subsubkey, NULL, &type,
                       (BYTE *) &value, &size) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegQueryValueEx");
      value = def;
    }

  if (RegCloseKey (reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegCloseKey");
    }
  return value;
}

/*
 * Write an unsigned integer value to the Windows Registry Database.
 */
void
svz_windoze_set_reg_unsigned (HKEY key, char *subkey,
                              char *subsubkey, unsigned value)
{
  DWORD size, type;
  HKEY reg;

  if (RegOpenKeyEx (key, subkey, 0, KEY_SET_VALUE, &reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegOpenKeyEx");
      return;
    }

  size = sizeof (DWORD);
  type = REG_DWORD;
  if (RegSetValueEx (reg, subsubkey, 0, type,
                     (BYTE *) &value, size) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegSetValueEx");
    }

  if (RegCloseKey (reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegCloseKey");
    }
}

/*
 * Read a string value from the Windows Registry Database.
 */
char *
svz_windoze_get_reg_string (HKEY key, char *subkey, char *subsubkey, char *def)
{
  static char value[128];
  DWORD size, type;
  HKEY reg;

  if (RegOpenKeyEx (key, subkey, 0, KEY_QUERY_VALUE, &reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegOpenKeyEx");
      return def;
    }

  size = sizeof (value);
  type = REG_SZ;
  if (RegQueryValueEx (reg, subsubkey, NULL, &type,
                       (BYTE *) value, &size) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegQueryValueEx");
      strcpy (value, def);
    }

  if (RegCloseKey (reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegCloseKey");
    }
  return value;
}

/*
 * Write a string value to the Windows Registry Database.
 */
void
svz_windoze_set_reg_string (HKEY key, char *subkey,
                            char *subsubkey, char *value)
{
  DWORD size, type;
  HKEY reg;

  if (RegOpenKeyEx (key, subkey, 0, KEY_SET_VALUE, &reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegOpenKeyEx");
      return;
    }

  size = strlen (value);
  type = REG_SZ;
  if (RegSetValueEx (reg, subsubkey, 0, type,
                     (BYTE *) value, size) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegSetValueEx");
    }

  if (RegCloseKey (reg) != ERROR_SUCCESS)
    {
      svz_log_sys_error ("RegCloseKey");
    }
}

/**
 * Convert an ASCII string into a UNICODE string.
 */
WCHAR *
svz_windoze_asc2uni (CHAR *asc)
{
  static WCHAR unicode[256];
  MultiByteToWideChar (CP_ACP, 0, asc, -1, unicode, 256);
  return unicode;
}

/**
 * Convert a UNICODE string into an ASCII string.
 */
CHAR *
svz_windoze_uni2asc (WCHAR *unicode)
{
  static CHAR asc[256];
  WideCharToMultiByte (CP_ACP, 0, unicode, -1, asc, 256, NULL, NULL);
  return asc;
}
