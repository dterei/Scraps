/* GHC_PACKAGES base integer ghc-prim rts
*/
#include "Stg.h"
EI_(base_GHCziBase_unpackCStringzh_closure);
static StgWord sse_srt[] = {
(W_)&base_GHCziBase_unpackCStringzh_closure
};

II_(sse_info);
static StgWord sse_closure[] = {
(W_)&sse_info, 0x0, 0x0, 0x0
};

static char cso_str[] = "hello";

static StgWord sse_info[] = {
((W_)&sse_srt+0), 0x0, 0x10016U
};

EI_(base_GHCziBase_unpackCStringzh_closure);
IF_(sse_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _csr;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _csr;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EI_(newCAF);
((void (*)(void *))(W_)&newCAF)((void *)R1.w);
R1.p[1] = (W_)Hp-4;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-4;
R1.w = (W_)&base_GHCziBase_unpackCStringzh_closure;
Sp[-3] = (W_)&cso_str;
Sp=Sp-3;
JMP_((W_)&stg_ap_n_fast);
_csr:
HpAlloc = 0x8U;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_SystemziIO_putStrLn_closure);
II_(sse_closure);
static StgWord Main_main_srt[] = {
(W_)&base_SystemziIO_putStrLn_closure, (W_)&sse_closure
};

EI_(Main_main_info);
StgWord Main_main_closure[] = {
(W_)&Main_main_info, 0x0, 0x0, 0x0
};

StgWord Main_main_info[] = {
((W_)&Main_main_srt+0), 0x0, 0x30016U
};

EI_(base_SystemziIO_putStrLn_closure);
II_(sse_closure);
FN_(Main_main_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _csB;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _csB;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EI_(newCAF);
((void (*)(void *))(W_)&newCAF)((void *)R1.w);
R1.p[1] = (W_)Hp-4;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-4;
R1.w = (W_)&base_SystemziIO_putStrLn_closure;
Sp[-3] = (W_)&sse_closure;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_csB:
HpAlloc = 0x8U;
JMP_(stg_gc_enter_1);
FE_
}
EI_(base_GHCziTopHandler_runMainIO_closure);
EI_(Main_main_closure);
static StgWord ZCMain_main_srt[] = {
(W_)&base_GHCziTopHandler_runMainIO_closure, (W_)&Main_main_closure
};

EI_(ZCMain_main_info);
StgWord ZCMain_main_closure[] = {
(W_)&ZCMain_main_info, 0x0, 0x0, 0x0
};

StgWord ZCMain_main_info[] = {
((W_)&ZCMain_main_srt+0), 0x0, 0x30016U
};

EI_(base_GHCziTopHandler_runMainIO_closure);
EI_(Main_main_closure);
FN_(ZCMain_main_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _csL;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _csL;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EI_(newCAF);
((void (*)(void *))(W_)&newCAF)((void *)R1.w);
R1.p[1] = (W_)Hp-4;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-4;
R1.w = (W_)&base_GHCziTopHandler_runMainIO_closure;
Sp[-3] = (W_)&Main_main_closure;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_csL:
HpAlloc = 0x8U;
JMP_(stg_gc_enter_1);
FE_
}
static StgWord _module_registered[] = {
0x0
};


EF_(__stginit_base_Prelude_);
EF_(__stginit_base_GHCziTopHandler_);
FN_(__stginit_Main_) {
FB_
if ((W_)(0x0 != (*((P_)(W_)&_module_registered)))) goto _csR;
goto _csT;
_csR:
Sp=Sp+1;
JMP_(Sp[-1]);
_csT:
*((P_)(W_)&_module_registered) = 0x1U;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_Prelude_;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_GHCziTopHandler_;
goto _csR;
FE_
}


EF_(__stginit_Main_);
FN_(__stginit_Main) {
FB_
JMP_((W_)&__stginit_Main_);
FE_
}


FN_(__stginit_ZCMain) {
FB_
Sp=Sp+1;
JMP_(Sp[-1]);
FE_
}
