### Vulnerability Details

There is a bug in the http_cgi_write function under http-cgi.c that causes an information leak.
The issue occurs on line 269 with this check:

  if (do_write > http->contentlength)
    do_write = http->contentlength;

The content length field is passed as a parameter in the content-length header of an http packet. However, the contentlength variable is an int, so it is signed.
On the other hand, do_write is an unsigned value. If we are able to make contentlength a negative value, the above check will pass but do_write will
be set to a huge unsigned value.

do_write is later used on line 280 for non-windows systems:

    if ((num_written = write (sock->pipe_desc[SVZ_WRITE],
                             sock->recv_buffer, do_write)) == -1)

do_write is the length of the write system call.
A large value in do_write will cause write() to send an excessive amount of data to the cgi process resulting in an information leak to that process or possibly a segmentation fault.

We can set contentlength to a negative value by suppling a sufficiently large unsigned value in a malicious http packet.

I recommend fixing this by changing the contentlength variable to an unsigned value in line 78 of http-core.h.

### Reproducing the Vulnerability

Go to /build/bin directory in one terminal and run an ASAN compiled 32-bit version of serveez with ./serveez
In another terminal, go to the attack_files folder and run ./leak.py leak\_pkt.dat

This should cause ASAN to report an error. This is because a heap buffer overread has occured. You should see an output somewhat similar to this:
```
=================================================================
==39286==ERROR: AddressSanitizer: heap-buffer-overflow on address 0xe8af4908 at pc 0x080d709e bp 0xfff31fc8 sp 0xfff31ba0
READ of size 53248 at 0xe8af4908 thread T0
=================================================================
==36108==ERROR: AddressSanitizer: heap-buffer-overflow on address 0xf5a226d3 at pc 0x08101a0a bp 0xfff31de8 sp 0xfff319c0
READ of size 14 at 0xf5a226d3 thread T0
    #0 0x8101a09 in __interceptor_memcmp /raid/agadient/llvm/compiler-rt/lib/asan/../sanitizer_common/sanitizer_common_interceptors.inc:774
    #0 0x80d709d in write /raid/agadient/llvm/compiler-rt/lib/asan/../sanitizer_common/sanitizer_common_interceptors.inc:1037
    #1 0x81cac39 in svz_envblock_add /raid/agadient/CVE/serveez-0.2.2/src/libserveez/passthrough.c:1244:10
    #1 0x81a9cef in http_cgi_write /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-cgi.c:280:22
    #2 0x81aaed0 in cgi_create_envp /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-cgi.c:413:15
    #2 0x81bc37a in check_sockets_select /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-loop.c:295:25
    #3 0x81aaed0 in pre_exec /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-cgi.c:583
    #3 0x81bc37a in svz_check_sockets /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-loop.c:883
    #4 0x81aaed0 in cgi_exec /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-cgi.c:797
    #5 0x81abb12 in http_post_response /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-cgi.c:1063:7
    #4 0x81babe7 in svz_loop_one /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-core.c:1040:3
    #6 0x81a5b34 in http_handle_request /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-proto.c:811:11
    #7 0x81a5449 in http_check_request /raid/agadient/CVE/serveez-0.2.2/src/http-server/http-proto.c:847:11
    #5 0x81bb4a4 in svz_loop /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-core.c:1147:7
    #6 0x816662d in guile_entry /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:122:3
    #8 0x81c1130 in svz_sock_detect_proto /raid/agadient/CVE/serveez-0.2.2/src/libserveez/socket.c:213:20
    #7 0x8165eef in guile_launch_pad /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:58:3
    #9 0x81b26d9 in svz_tcp_read_socket /raid/agadient/CVE/serveez-0.2.2/src/libserveez/tcp-socket.c:179:22
    #10 0x81bc4a8 in check_sockets_select /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-loop.c:330:21
    #11 0x81bc4a8 in svz_check_sockets /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-loop.c:883
    #12 0x81babe7 in svz_loop_one /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-core.c:1040:3
    #13 0x81bb4a4 in svz_loop /raid/agadient/CVE/serveez-0.2.2/src/libserveez/server-core.c:1147:7
    #14 0x816662d in guile_entry /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:122:3
    #15 0x8165eef in guile_launch_pad /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:58:3
    #8 0xf7dac4ac  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x644ac)
    #9 0xf7d8309c  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b09c)
    #10 0xf7e0ca96  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4a96)
    #16 0xf7dac4ac  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x644ac)
    #11 0xf7de6507  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x9e507)
    #17 0xf7d8309c  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b09c)
    #12 0xf7e256c7  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xdd6c7)
    #18 0xf7e0ca96  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4a96)
    #13 0xf7e25a38  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xdda38)
    #19 0xf7de6507  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x9e507)
    #14 0xf7d8d902 in scm_call_4 (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x45902)
    #20 0xf7e256c7  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xdd6c7)
    #15 0xf7e0cc6e in scm_catch_with_pre_unwind_handler (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4c6e)
    #21 0xf7e25a38  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xdda38)
    #16 0xf7e0cd73 in scm_c_catch (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4d73)
    #22 0xf7d8d902 in scm_call_4 (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x45902)
    #17 0xf7d83873  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b873)
    #23 0xf7e0cc6e in scm_catch_with_pre_unwind_handler (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4c6e)
    #18 0xf7d83972 in scm_c_with_continuation_barrier (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b972)
    #24 0xf7e0cd73 in scm_c_catch (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc4d73)
    #19 0xf7e0a0cd  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc20cd)
    #25 0xf7d83873  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b873)
    #26 0xf7d83972 in scm_c_with_continuation_barrier (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x3b972)
    #27 0xf7e0a0cd  (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc20cd)
    #28 0xf7d08620 in GC_call_with_stack_base (/usr/lib/i386-linux-gnu/libgc.so.1+0x18620)
    #29 0xf7e0a535 in scm_with_guile (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc2535)
    #20 0xf7d08620 in GC_call_with_stack_base (/usr/lib/i386-linux-gnu/libgc.so.1+0x18620)
    #30 0xf7dac6fb in scm_boot_guile (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x646fb)
    #21 0xf7e0a535 in scm_with_guile (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0xc2535)
    #22 0xf7dac6fb in scm_boot_guile (/usr/lib/i386-linux-gnu/libguile-2.0.so.22+0x646fb)
    #31 0x8166239 in main /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:304:3
    #23 0x8166239 in main /raid/agadient/CVE/serveez-0.2.2/src/serveez.c:304:3
    #32 0xf7ab4636 in __libc_start_main /build/glibc-GoSbp4/glibc-2.23/csu/../csu/libc-start.c:291
    #24 0xf7ab4636 in __libc_start_main /build/glibc-GoSbp4/glibc-2.23/csu/../csu/libc-start.c:291
    #33 0x8067307 in _start (/raid/agadient/CVE/build/bin/serveez+0x8067307)
    #25 0x8067307 in _start (/raid/agadient/CVE/build/bin/serveez+0x8067307)


0xf5a226d3 is located 0 bytes to the right of 19-byte region [0xf5a226c0,0xf5a226d3)
0xe8af4908 is located 0 bytes to the right of 8200-byte region [0xe8af2900,0xe8af4908)
allocated by thread T0 here:
allocated by thread T0 here:

    #0 0x812a9f4 in malloc /raid/agadient/llvm/compiler-rt/lib/asan/asan_malloc_linux.cc:67
    #0 0x812a9f4 in malloc /raid/agadient/llvm/compiler-rt/lib/asan/asan_malloc_linux.cc:67
    #1 0x81ae5de in svz_malloc /raid/agadient/CVE/serveez-0.2.2/src/libserveez/alloc.c:161:23
    #2 0x81ae5de in svz_strdup /raid/agadient/CVE/serveez-0.2.2/src/libserveez/alloc.c:382

    #1 0x81ae213 in svz_malloc /raid/agadient/CVE/serveez-0.2.2/src/libserveez/alloc.c:161:23

SUMMARY: AddressSanitizer: heap-buffer-overflow /raid/agadient/llvm/compiler-rt/lib/asan/../sanitizer_common/sanitizer_common_interceptors.inc:774 in __interceptor_memcmp
SUMMARY: AddressSanitizer: heap-buffer-overflow /raid/agadient/llvm/compiler-rt/lib/asan/../sanitizer_common/sanitizer_common_interceptors.inc:1037 in write
Shadow bytes around the buggy address:
  0x3eb44480: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3eb44490: fa fa fa fa fa fa fa fa fa fa fa fa 00 00 04 fa
  0x3eb444a0: fa fa 00 00 00 01 fa fa 00 00 00 fa fa fa 00 00
  0x3eb444b0: 05 fa fa fa 00 00 03 fa fa fa 00 00 00 fa fa fa
  0x3eb444c0: 00 00 00 00 fa fa 00 00 06 fa fa fa 00 00 00 03
=>0x3eb444d0: fa fa 00 00 07 fa fa fa 00 00[03]fa fa fa 00 00
  0x3eb444e0: 00 01 fa fa fd fd fd fd fa fa 00 00 00 03 fa fa
  0x3eb444f0: fd fd fd fd fa fa fd fd fd fa fa fa 00 00 00 04
  0x3eb44500: fa fa fd fd fd fa fa fa 00 00 00 fa fa fa 00 00
  0x3eb44510: 04 fa fa fa 00 00 00 fa fa fa 00 00 03 fa fa fa
  0x3eb44520: 00 00 07 fa fa fa 00 00 00 fa fa fa 00 00 04 fa
Shadow byte legend (one shadow byte represents 8 application bytes):
  Addressable:           00
  Partially addressable: 01 02 03 04 05 06 07
  Heap left redzone:       fa
  Freed heap region:       fd
  Stack left redzone:      f1
  Stack mid redzone:       f2
  Stack right redzone:     f3
  Stack after return:      f5
  Stack use after scope:   f8
  Global redzone:          f9
  Global init order:       f6
  Poisoned by user:        f7
  Container overflow:      fc
  Array cookie:            ac
  Intra object redzone:    bb
  ASan internal:           fe
  Left alloca redzone:     ca
  Right alloca redzone:    cb
Shadow bytes around the buggy address:
  0x3d15e8d0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x3d15e8e0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x3d15e8f0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x3d15e900: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x3d15e910: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
=>0x3d15e920: 00[fa]fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3d15e930: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3d15e940: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3d15e950: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3d15e960: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x3d15e970: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
Shadow byte legend (one shadow byte represents 8 application bytes):
  Addressable:           00
  Partially addressable: 01 02 03 04 05 06 07
  Heap left redzone:       fa
  Freed heap region:       fd
  Stack left redzone:      f1
  Stack mid redzone:       f2
  Stack right redzone:     f3
  Stack after return:      f5
  Stack use after scope:   f8
  Global redzone:          f9
  Global init order:       f6
  Poisoned by user:        f7
  Container overflow:      fc
  Array cookie:            ac
  Intra object redzone:    bb
  ASan internal:           fe
  Left alloca redzone:     ca
  Right alloca redzone:    cb
==36108==ABORTING
==39286==ABORTING
```
