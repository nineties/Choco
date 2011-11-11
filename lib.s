lib_read:
  li r4 0;
  li r2 0;
read_get_retry:
  get r3;
	nop;
	bli r3 0 read_get_retry;
  adds r2 r2 r3;
  bnei r4 3 read_next;
  ret;
    nop;
read_next:
  slliu r2 r2 8;
  jumpi read_get_retry;
    addiu r4 r4 1;
lib_create_array:
  adds r5 r3 r0;
  adds r2 r62 r0;
create_array_loop:
  ble r5 r0 create_array_end;
  storei r4 r62 0;
  subiu r5 r5 1;
  jumpi create_array_loop;
    addiu r62 r62 1;
create_array_end:
  ret;
    nop;
lib_floor:
  subiu r1 r1 1;
  storei r3 r1 0;
  calli lib_int_of_float;
    nop;
  adds r3 r2 r0;
  calli lib_float_of_int;
    nop;
  loadi r3 r1 0;
  addiu r1 r1 1;
  blf r3 r0 floor_neg;
floor_normal:
  ret;
    nop;
floor_neg:
  bef r2 r3 floor_normal;
  nop;
  hsr r4 1; /* libconst_one */
  fsub r2 r2 r4;
  ret;
    nop;
lib_float_of_int:
  hsr r6 57; // float 8388608
	fabs  r4 r3;
	subs  r4 r3 r4;
	bgi   r3 0 itof_no_iabs;
	subs r3 r0 r3;
itof_no_iabs:
	adds  r5 r3 r6;
	fsub  r5 r5 r6;
  hsr r6 58; // int 8388608
	bg    r3 r6 itof_shiftRight;
	ret;
		adds r2 r4 r5;
itof_shiftRight:
	ret;
		addiu r2 r0 0;
lib_int_of_float:
  hsr r6 57; // float 8388608
	fabs  r4 r3;
	bg    r4 r6 ftoi_shiftLeft0;
	fadd  r4 r4 r6;
		nop;
		nop;
	subs  r4 r4 r6;
	blei  r3 0 ftoi_neg;
	ret;
		addiu r2 r4 0;
ftoi_neg:
	ret;
		subs r2 r0 r4;
ftoi_shiftLeft0:
	ret;
		addiu r2 r0 0;
lib_sin:
  subiu r1 r1 1;
  storei r3 r1 0;
  fabs r4 r3;
  adds r5 r0 r0; // r
  hsr r6 35; // quarter pi
  hsr r7 36; // half pi
sin_reduction_loop:
  blf r4 r6 sin_reduction_loopend;
  fsub r4 r4 r7;
  bgei r5 3 sin_reduction_loop_rreset;
  jumpi sin_reduction_loop;
    addiu r5 r5 1;
sin_reduction_loop_rreset:
  jumpi sin_reduction_loop;
    adds r5 r0 r0;
sin_reduction_loopend:
  bnei r5 0 sin_rem_not0;
sin_rem0:
  calli k_sin;
    adds r3 r4 r0;
  loadi r3 r1 0;
  addiu r1 r1 1;
sin_ret:
  ble r3 r0 sin_ret_neg;
  ret;
    nop;
sin_ret_neg:
  ret;
    fneg r2 r2;
sin_rem_not0:
  bnei r5 1 sin_rem_not1;
  calli k_cos;
    adds r3 r4 r0;
  loadi r3 r1 0;
  jumpi sin_ret;
    addiu r1 r1 1;
sin_rem_not1:
  bnei r5 2 sin_rem_not2;
  calli k_sin;
    adds r3 r4 r0;
  fneg r2 r2;
  loadi r3 r1 0;
  jumpi sin_ret;
    addiu r1 r1 1;
sin_rem_not2:
  calli k_cos;
    adds r3 r4 r0;
  fneg r2 r2;
  loadi r3 r1 0;
  jumpi sin_ret;
    addiu r1 r1 1;
  
lib_cos:
  fabs r3 r3;
  adds r4 r0 r0; // r
  hsr r5 35; // quarter pi
  hsr r6 36; // half pi
cos_reduction_loop:
  blf r3 r5 cos_reduction_loopend;
  fsub r3 r3 r6;
  bgei r4 3 cos_reduction_loop_rreset;
  jumpi cos_reduction_loop;
    addiu r4 r4 1;
cos_reduction_loop_rreset:
  jumpi cos_reduction_loop;
    adds r4 r0 r0;
cos_reduction_loopend:
  bnei r4 0 cos_rem_not0;
cos_rem0:
  calli k_cos;
    nop;
  ret;
    nop;
cos_rem_not0:
  bnei r4 1 cos_rem_not1;
  calli k_sin;
    nop;
  ret;
    fneg r2 r2;
cos_rem_not1:
  bnei r4 2 cos_rem_not2;
  calli k_cos;
    nop;
  ret;
    fneg r2 r2;
cos_rem_not2:
  calli k_sin;
    nop;
  ret;
    nop;

k_sin:
  fmul r4 r3 r3;
  hsr r5 48;
  nop;
  fmul r7 r4 r5;
  hsr r6 47;
  nop;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r5 46;
  nop;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r7 r4 r7;
  nop;
  hsr r6 45;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  nop;
  hsr r5 44;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r7 r4 r7;
  nop;
  hsr r6 43;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  nop;
  hsr r5 42;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r2 r7 r3;
  ret;
    nop;

k_cos:
  fmul r4 r3 r3;
  hsr r7 56;
  nop;
  fmul r7 r4 r7;
  hsr r5 55;
  nop;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r6 54;
  nop;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r5 53;
  nop;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r6 52;
  nop;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r5 51;
  nop;
  fadd r7 r7 r5;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r6 50;
  nop;
  fadd r7 r7 r6;
  nop;
  nop;
  fmul r7 r4 r7;
  hsr r5 49;
  nop;
  fadd r2 r7 r5;
  ret;
    nop;
lib_atan:
  fabs r6 r3;
  hsr r7 10; // libconst_pow_2_34
  bgef r6 r7 atan_verybig;
  nop;
  hsr r7 11; // libconst_pow_2_m29
  blf r6 r7 atan_verysmall;
  nop;
  hsr r7 12; // libconst_atan_04375
  blf r6 r7 atan_small;
  adds r5 r3 r0; /* hx */
  fabs r3 r3;
  hsr r7 13; // libconst_atan_06875
  blf r6 r7 atan_medium1;
  nop;
  hsr r7 14; // libconst_atan_11875
  blf r6 r7 atan_medium2;
  nop;
  hsr r7 15; // libconst_atan_24375
  blf r6 r7 atan_medium3;
atan_big: /* live_reg : r3 r5 */
  subiu r1 r1 1;
  storei r5 r1 0;
  finv r2 r3;
  nop;
  nop;
  fneg r3 r2;
  subiu r1 r1 1;
  storei r3 r1 1;
  calli atan_kernel;
    nop;
  loadi r5 r1 0;
  addiu r1 r1 1;
  fmul r4 r2 r5;
  hsr r7 23; // libconst_atan_lo(3)
  hsr r6 19; // libconst_atan_hi(3)
  fsub r4 r4 r7;
  nop;
  nop;
  fsub r4 r4 r5;
  nop;
  nop;
  fsub r2 r6 r4; /* z : r4 */
  loadi r5 r1 0;
  addiu r1 r1 1;
  bl r5 r0 atan_return_neg;
  ret;
    nop;
atan_verybig:
  blf r3 r0 atan_verybig_neg;
  ret;
    hsr r2 8; // libconst_atan_big_p
atan_verybig_neg:
  ret;
    hsr r2 9; // libconst_atan_big_n
atan_verysmall:
  ret;
    adds r2 r3 r0;
atan_small:
  subiu r1 r1 1;
  storei r3 r1 0;
  calli atan_kernel;
    nop;
  loadi r5 r1 0;
  nop;
  fmul r6 r2 r5;
  nop;
  nop;
  fsub r2 r5 r6;
  ret;
    addiu r1 r1 1;
atan_medium1:
  subiu r1 r1 1;
  storei r5 r1 0;
  nop;
  nop;
  hsr r7 3; // libconst_two;
  fmul r5 r3 r7;
  hsr r6 1; // libconst_one;
  nop;
  fsub r4 r5 r6;
  fadd r3 r3 r7;
  nop;
  nop;
  finv r3 r3;
  nop;
  nop;
  fmul r2 r3 r4;
  nop;
  subiu r1 r1 1;
  storei r2 r1 0;
  calli atan_kernel;
    adds r3 r2 r0;
  loadi r5 r1 0; /* r5 : x, r4 : kernel(x) */
  addiu r1 r1 1;
  fmul r4 r2 r5;
  hsr r8 16;
  hsr r9 20;
  fsub r4 r4 r9;
  nop;
  nop;
  fsub r4 r4 r5;
  loadi r5 r1 0;
  addiu r1 r1 1;
  fsub r2 r8 r4;
  bl r5 r0 atan_return_neg;
  ret;
    nop;
atan_medium2:
  hsr r6 1; // libconst_one
  subiu r1 r1 1;
  storei r5 r1 0;
  fsub r5 r3 r6;
  fadd r3 r3 r6;
  nop;
  nop;
  finv r3 r3;
  nop;
  nop;
  fmul r2 r3 r4;
  nop;
  subiu r1 r1 1;
  storei r2 r1 0;
  calli atan_kernel;
    adds r3 r2 r0;
  loadi r5 r1 0;
  addiu r1 r1 1;
  fmul r4 r2 r5;
  hsr r9 21;
  hsr r8 17;
  fsub r4 r4 r9;
  nop;
  nop;
  fsub r4 r4 r5;
  loadi r5 r1 0;
  addiu r1 r1 1;
  fsub r2 r8 r4;
  bl r5 r0 atan_return_neg;
  ret;
    nop;
atan_medium3: /* live_reg : r4 r5 */
  hsr r7 2; // libconst_one_half
  subiu r1 r1 1;
  storei r5 r1 0;
  fmul r10 r7 r3;
  fsub r4 r3 r7;
  hsr r6 1; // libconst_one
  fadd r3 r6 r10;
  nop;
  nop;
  finv r3 r3;
  nop;
  nop;
  fmul r2 r3 r4;
  nop;
  subiu r1 r1 1;
  storei r2 r1 0;
  calli atan_kernel;
    adds r3 r2 r0;
  loadi r5 r1 0; /* r5 : x, r4 : kernel(x) */
  addiu r1 r1 1;
  fmul r10 r2 r5;
  hsr r9 22; // libconst_atan_lo(2)
  hsr r8 18; // libconst_atan_hi(2)
  fsub r10 r10 r9;
  nop;
  nop;
  fsub r10 r10 r5;
  loadi r5 r1 0;
  addiu r1 r1 1;
  fsub r2 r8 r10; /* r4 : z */
  bl r5 r0 atan_return_neg;
  ret;
    nop;
atan_return_neg:
  fsub r2 r0 r2;
  ret;
    nop;
atan_kernel: /* r3 : x, r4 : z, r5 : w, r6 : s1, r7 : s2 */
  fmul r4 r3 r3;
  hsr r9 34;
  hsr r8 33;
  fmul r5 r4 r4;
  hsr r9 32;
  hsr r8 31;
  fmul r6 r5 r9;
  fmul r7 r5 r8;
  hsr r9 30;
  fadd r6 r6 r9;
  fadd r7 r7 r8;
  hsr r8 29;
  fmul r6 r5 r6;
  fmul r7 r5 r7;
  hsr r9 28;
  fadd r6 r6 r9;
  fadd r7 r7 r8;
  hsr r8 27;
  fmul r6 r5 r6;
  fmul r7 r5 r7;
  hsr r9 26;
  fadd r6 r6 r9;
  fadd r7 r7 r8;
  hsr r8 25;
  fmul r6 r5 r6;
  fmul r7 r5 r7;
  hsr r9 24;
  fadd r6 r6 r9;
  fadd r7 r7 r8;
  nop;
  fmul r6 r5 r6;
  fmul r7 r5 r7;
  nop;
  fadd r6 r6 r9;
  nop;
  nop;
  fmul r6 r6 r4;
  nop;
  nop;
  fadd r2 r6 r7;
  ret;
    nop;
libconst_half:
  .float 0.5;
libconst_one:
  .float 1.0;
libconst_one_half:
  .float 1.5;
libconst_two:
  .float 2.0;
libconst_sqrt_0:
  .float 0x3ec00000;
libconst_sqrt_1:
  .float 0x3ea00000;
libconst_sqrt_2:
  .float 0x3e8c0000;
libconst_sqrt_3:
  .float 0x3e7c0000;
libconst_atan_big_p:
  .float 0x3fc90fdb;
libconst_atan_big_n:
  .float 0xbfc90fdb;
libconst_atan_hi:
  .float 4.6364760399e-01;
  .float 7.8539312565e-01;
  .float 9.8279368877e-01;
  .float 1.5707962513e+00;
libconst_atan_lo:
  .float 5.0121582440e-09;
  .float 3.7748947079e-08;
  .float 3.4473217170e-08;
  .float 7.5497894159e-08;
libconst_pow_2_34:
  .float 0x50800000;
libconst_pow_2_m29:
  .float 0x31000000;
libconst_atan_04375:
  .float 0x3ee00000;
libconst_atan_06875:
  .float 0x3f300000;
libconst_atan_11875:
  .float 0x3f980000;
libconst_atan_24375:
  .float 0x401c0000;
libconst_atan_table:
  .float 0x3eaaaaaa;
  .float 0xbe4ccccd;
  .float 0x3e124925;
  .float 0xbde38e38;
  .float 0x3dba2e6e;
  .float 0xbd9d8795;
  .float 0x3d886b35;
  .float 0xbd6ef16b;
  .float 0x3d4bda59;
  .float 0xbd15a221;
  .float 0x3c8569d7;
libconst_quarter_pi:
  .float 0x3f490fdb;
libconst_half_pi:
  .float 0x3fc90fdb;
libconst_three_quarter_pi:
  .float 0x4016cbe4;
libconst_pi:
  .float 0x40490fdb;
libconst_double_pi:
  .float 0x40c90fdb;
libconst_sin_table:
  .float 0x3f800000;
  .float 0xbe2aaaab;
  .float 0x3c088889;
  .float 0xb9500d01;
  .float 0x3638ef1b;
  .float 0xb2d72f34;
  .float 0x2f2ec9d3;
libconst_cos_table:
  .float 0x3f800000;
  .float 0xbf000000;
  .float 0x3d2aaaab;
  .float 0xbab60b61;
  .float 0x37d00d01;
  .float 0xb493f27c;
  .float 0x310f74f6;
  .float 0xad47d74e;
libconst_flt8388608:
  .float 0x4b000000;
libconst_int8388608:
  .float 0x00800000;
libconst_inf:
  .float 0x7f800000;
lib_const_load:
  loadi r3 r60 (libconst_half - 5000);
  loadi r4 r60 (libconst_one - 5000);
  loadi r5 r60 (libconst_one_half - 5000);
  loadi r6 r60 (libconst_two - 5000);
  nop;
  nop;
  hsw r3 0;
  hsw r4 1;
  hsw r5 2;
  hsw r6 3;
  loadi r3 r60 (libconst_sqrt_0 - 5000);
  loadi r4 r60 (libconst_sqrt_1 - 5000);
  loadi r5 r60 (libconst_sqrt_2 - 5000);
  loadi r6 r60 (libconst_sqrt_3 - 5000);
  nop;
  nop;
  hsw r3 4;
  hsw r4 5;
  hsw r5 6; 
  hsw r6 7;
  loadi r3 r60 (libconst_atan_big_p - 5000);
  loadi r4 r60 (libconst_atan_big_n - 5000);
  nop;
  nop;
  hsw r3 8;
  hsw r4 9;
  loadi r3 r60 (libconst_pow_2_34 - 5000);
  loadi r4 r60 (libconst_pow_2_m29 - 5000);
  loadi r5 r60 (libconst_atan_04375 - 5000);
  loadi r6 r60 (libconst_atan_06875 - 5000);
  loadi r7 r60 (libconst_atan_11875 - 5000);
  loadi r8 r60 (libconst_atan_24375 - 5000);
  nop;
  nop;
  hsw r3 10;
  hsw r4 11;
  hsw r5 12; 
  hsw r6 13;
  hsw r7 14;
  hsw r8 15;
  loadi r3 r60 (libconst_atan_hi + 0 - 5000);
  loadi r4 r60 (libconst_atan_hi + 1 - 5000);
  loadi r5 r60 (libconst_atan_hi + 2 - 5000);
  loadi r6 r60 (libconst_atan_hi + 3 - 5000);
  nop;
  nop;
  hsw r3 16;
  hsw r4 17;
  hsw r5 18;
  hsw r6 19;
  loadi r3 r60 (libconst_atan_lo + 0 - 5000);
  loadi r4 r60 (libconst_atan_lo + 1 - 5000);
  loadi r5 r60 (libconst_atan_lo + 2 - 5000);
  loadi r6 r60 (libconst_atan_lo + 3 - 5000);
  nop;
  nop;
  hsw r3 20;
  hsw r4 21;
  hsw r5 22;
  hsw r6 23;
  loadi r3 r60 (libconst_atan_table + 0 - 5000);
  loadi r4 r60 (libconst_atan_table + 1 - 5000);
  loadi r5 r60 (libconst_atan_table + 2 - 5000);
  loadi r6 r60 (libconst_atan_table + 3 - 5000);
  loadi r7 r60 (libconst_atan_table + 4 - 5000);
  loadi r8 r60 (libconst_atan_table + 5 - 5000);
  loadi r9 r60 (libconst_atan_table + 6 - 5000);
  loadi r10 r60 (libconst_atan_table + 7 - 5000);
  loadi r11 r60 (libconst_atan_table + 8 - 5000);
  loadi r12 r60 (libconst_atan_table + 9 - 5000);
  loadi r13 r60 (libconst_atan_table + 10 - 5000);
  nop;
  nop;
  hsw r3 24;
  hsw r4 25;
  hsw r5 26; 
  hsw r6 27;
  hsw r7 28;
  hsw r8 29;
  hsw r9 30;
  hsw r10 31;
  hsw r11 32;
  hsw r12 33;
  hsw r13 34;
  loadi r3 r60 (libconst_quarter_pi - 5000);
  loadi r4 r60 (libconst_half_pi - 5000);
  loadi r5 r60 (libconst_three_quarter_pi - 5000);
  loadi r6 r60 (libconst_pi - 5000);
  loadi r7 r60 (libconst_double_pi - 5000);
  nop;
  nop;
  hsw r3 35;
  fsub r8 r60 r3;
  hsw r4 36;
  hsw r5 37;
  fsub r9 r60 r5;
  hsw r6 38;
  hsw r7 39;
  hsw r8 40;
  hsw r9 41;
  loadi r3 r60 (libconst_sin_table + 0 - 5000);
  loadi r4 r60 (libconst_sin_table + 1 - 5000);
  loadi r5 r60 (libconst_sin_table + 2 - 5000);
  loadi r6 r60 (libconst_sin_table + 3 - 5000);
  loadi r7 r60 (libconst_sin_table + 4 - 5000);
  loadi r8 r60 (libconst_sin_table + 5 - 5000);
  loadi r9 r60 (libconst_sin_table + 6 - 5000);
  nop;
  nop;
  hsw r3 42;
  hsw r4 43;
  hsw r5 44;
  hsw r6 45;
  hsw r7 46;
  hsw r8 47;
  hsw r9 48;
  loadi r3 r60 (libconst_cos_table + 0 - 5000);
  loadi r4 r60 (libconst_cos_table + 1 - 5000);
  loadi r5 r60 (libconst_cos_table + 2 - 5000);
  loadi r6 r60 (libconst_cos_table + 3 - 5000);
  loadi r7 r60 (libconst_cos_table + 4 - 5000);
  loadi r8 r60 (libconst_cos_table + 5 - 5000);
  loadi r9 r60 (libconst_cos_table + 6 - 5000);
  loadi r10 r60 (libconst_cos_table + 7 - 5000);
  nop;
  nop;
  hsw r3 49;
  hsw r4 50;
  hsw r5 51;
  hsw r6 52;
  hsw r7 53;
  hsw r8 54;
  hsw r9 55;
  hsw r10 56;
  loadi r3 r60 (libconst_flt8388608 - 5000);
  loadi r4 r60 (libconst_int8388608 - 5000);
  loadi r5 r60 (libconst_inf - 5000);
  nop;
  nop;
  hsw r3 57;
  hsw r4 58;
  hsw r5 59;
  ret;
    nop;
