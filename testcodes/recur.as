/* Generated by choco in Mon Jan 21 22:15:15 JST 2008*/
/* need two nops first */
	nop;
	nop;
	li r1 768;
	slliu r1 r1 4;  /* initialize stack pointer */
	move r62 r1;    /* initialize heap pointer */
/* initialize constants */

entry_point1014:
L.1016:
	li r3 5;
	li r4 2;
	calli fact_1004;
	li r5 1;
	calli lib_print_int;
	move r3 r2;
	ret;
	nop;
L.1017:
	halt;
	jumpi L.1017;
	nop;
fact_1000:
L.1018:
	ble r5 r3 L.99;
	move r2 r4;
	ret;
	nop;
L.99:
	addiu r39 r5 1;
	mul r4 r4 r5;
	jumpi L.1018;
	move r5 r39;
fact_1004:
L.1019:
	ble r4 r3 L.99;
	move r2 r5;
	ret;
	nop;
L.99:
	mul r5 r5 r4;
	jumpi L.1019;
	addiu r4 r4 1;
/* constants */
/* Code end */