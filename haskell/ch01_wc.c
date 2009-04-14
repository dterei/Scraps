/* GHC_PACKAGES base integer ghc-prim rts
*/
#include "Stg.h"
EI_(base_GHCziBase_zpzp_closure);
EI_(base_GHCziList_length_closure);
EI_(base_DataziList_lines_closure);
EI_(base_DataziList_words_closure);
EI_(base_GHCziShow_zdf16_closure);
static StgWord sye_srt[] = {
(W_)&base_GHCziBase_zpzp_closure, (W_)&base_GHCziList_length_closure, (W_)&base_DataziList_lines_closure, (W_)&base_DataziList_words_closure, (W_)&base_GHCziShow_zdf16_closure
};

II_(sye_info);
static StgWord sye_closure[] = {
(W_)&sye_info, 0x0
};

static StgWord sxW_info[] = {
((W_)&sye_srt+4), 0x1U, 0x10011U
};

EI_(base_GHCziList_length_closure);
IF_(sxW_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _cyD;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[2];
R1.w = (W_)&base_GHCziList_length_closure;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_cyD:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxY_info[] = {
((W_)&sye_srt+4), 0x1U, 0x90011U
};

EI_(base_GHCziShow_show_info);
EI_(base_GHCziShow_zdf16_closure);
II_(sxW_info);
IF_(sxY_entry) {
FB_
if ((W_)(((W_)Sp - 0x14U) < (W_)SpLim)) goto _cyG;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cyG;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&sxW_info;
*Hp = R1.p[2];
Sp[-3] = (W_)Hp-8;
Sp[-4] = (W_)&stg_ap_p_info;
Sp[-5] = (W_)&base_GHCziShow_zdf16_closure;
Sp=Sp-5;
JMP_((W_)&base_GHCziShow_show_info);
_cyG:
HpAlloc = 0xcU;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sy4_info[] = {
((W_)&sye_srt+0), 0x1U, 0x130011U
};

EI_(base_GHCziBase_zpzp_closure);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(sxY_info);
IF_(sy4_entry) {
FB_
if ((W_)(((W_)Sp - 0x10U) < (W_)SpLim)) goto _cyJ;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cyJ;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-4] = (W_)&stg_CHARLIKE_closure+81;
Hp[-3] = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
Hp[-2] = (W_)&sxY_info;
*Hp = R1.p[2];
R1.w = (W_)&base_GHCziBase_zpzp_closure;
Sp[-3] = (W_)Hp-18;
Sp[-4] = (W_)Hp-8;
Sp=Sp-4;
JMP_((W_)&stg_ap_pp_fast);
_cyJ:
HpAlloc = 0x18U;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sy6_info[] = {
((W_)&sye_srt+0), 0x1U, 0x130011U
};

EI_(base_GHCziBase_zpzp_closure);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(sy4_info);
IF_(sy6_entry) {
FB_
if ((W_)(((W_)Sp - 0x10U) < (W_)SpLim)) goto _cyM;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cyM;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&sy4_info;
Hp[-3] = R1.p[2];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&stg_CHARLIKE_closure+73;
*Hp = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)&base_GHCziBase_zpzp_closure;
Sp[-3] = (W_)Hp-20;
Sp[-4] = (W_)Hp-6;
Sp=Sp-4;
JMP_((W_)&stg_ap_pp_fast);
_cyM:
HpAlloc = 0x18U;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxM_info[] = {
((W_)&sye_srt+12), 0x1U, 0x10011U
};

EI_(base_DataziList_words_closure);
IF_(sxM_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _cyV;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[2];
R1.w = (W_)&base_DataziList_words_closure;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_cyV:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxO_info[] = {
((W_)&sye_srt+4), 0x1U, 0x50011U
};

EI_(base_GHCziList_length_closure);
II_(sxM_info);
IF_(sxO_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _cyY;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cyY;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&sxM_info;
*Hp = R1.p[2];
R1.w = (W_)&base_GHCziList_length_closure;
Sp[-3] = (W_)Hp-8;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_cyY:
HpAlloc = 0xcU;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxQ_info[] = {
((W_)&sye_srt+4), 0x1U, 0xd0011U
};

EI_(base_GHCziShow_show_info);
EI_(base_GHCziShow_zdf16_closure);
II_(sxO_info);
IF_(sxQ_entry) {
FB_
if ((W_)(((W_)Sp - 0x14U) < (W_)SpLim)) goto _cz1;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cz1;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&sxO_info;
*Hp = R1.p[2];
Sp[-3] = (W_)Hp-8;
Sp[-4] = (W_)&stg_ap_p_info;
Sp[-5] = (W_)&base_GHCziShow_zdf16_closure;
Sp=Sp-5;
JMP_((W_)&base_GHCziShow_show_info);
_cz1:
HpAlloc = 0xcU;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sy8_info[] = {
((W_)&sye_srt+0), 0x1U, 0x1b0011U
};

EI_(base_GHCziBase_zpzp_closure);
II_(sxQ_info);
II_(sy6_info);
IF_(sy8_entry) {
FB_
if ((W_)(((W_)Sp - 0x10U) < (W_)SpLim)) goto _cz4;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cz4;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&sy6_info;
Hp[-3] = R1.p[2];
Hp[-2] = (W_)&sxQ_info;
*Hp = R1.p[2];
R1.w = (W_)&base_GHCziBase_zpzp_closure;
Sp[-3] = (W_)Hp-20;
Sp[-4] = (W_)Hp-8;
Sp=Sp-4;
JMP_((W_)&stg_ap_pp_fast);
_cz4:
HpAlloc = 0x18U;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sya_info[] = {
((W_)&sye_srt+0), 0x1U, 0x1b0011U
};

EI_(base_GHCziBase_zpzp_closure);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(sy8_info);
IF_(sya_entry) {
FB_
if ((W_)(((W_)Sp - 0x10U) < (W_)SpLim)) goto _cz7;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _cz7;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&sy8_info;
Hp[-3] = R1.p[2];
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&stg_CHARLIKE_closure+73;
*Hp = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)&base_GHCziBase_zpzp_closure;
Sp[-3] = (W_)Hp-20;
Sp[-4] = (W_)Hp-6;
Sp=Sp-4;
JMP_((W_)&stg_ap_pp_fast);
_cz7:
HpAlloc = 0x18U;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxC_info[] = {
((W_)&sye_srt+8), 0x1U, 0x10011U
};

EI_(base_DataziList_lines_closure);
IF_(sxC_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _czg;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Sp[-3] = R1.p[2];
R1.w = (W_)&base_DataziList_lines_closure;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_czg:
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxE_info[] = {
((W_)&sye_srt+4), 0x1U, 0x30011U
};

EI_(base_GHCziList_length_closure);
II_(sxC_info);
IF_(sxE_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _czj;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czj;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&sxC_info;
*Hp = R1.p[2];
R1.w = (W_)&base_GHCziList_length_closure;
Sp[-3] = (W_)Hp-8;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_czj:
HpAlloc = 0xcU;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sxG_info[] = {
((W_)&sye_srt+4), 0x1U, 0xb0011U
};

EI_(base_GHCziShow_show_info);
EI_(base_GHCziShow_zdf16_closure);
II_(sxE_info);
IF_(sxG_entry) {
FB_
if ((W_)(((W_)Sp - 0x14U) < (W_)SpLim)) goto _czm;
Hp=Hp+3;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czm;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-2] = (W_)&sxE_info;
*Hp = R1.p[2];
Sp[-3] = (W_)Hp-8;
Sp[-4] = (W_)&stg_ap_p_info;
Sp[-5] = (W_)&base_GHCziShow_zdf16_closure;
Sp=Sp-5;
JMP_((W_)&base_GHCziShow_show_info);
_czm:
HpAlloc = 0xcU;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord syc_info[] = {
((W_)&sye_srt+0), 0x1U, 0x1f0011U
};

EI_(base_GHCziBase_zpzp_closure);
II_(sxG_info);
II_(sya_info);
IF_(syc_entry) {
FB_
if ((W_)(((W_)Sp - 0x10U) < (W_)SpLim)) goto _czp;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czp;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = R1.w;
Hp[-5] = (W_)&sya_info;
Hp[-3] = R1.p[2];
Hp[-2] = (W_)&sxG_info;
*Hp = R1.p[2];
R1.w = (W_)&base_GHCziBase_zpzp_closure;
Sp[-3] = (W_)Hp-20;
Sp[-4] = (W_)Hp-8;
Sp=Sp-4;
JMP_((W_)&stg_ap_pp_fast);
_czp:
HpAlloc = 0x18U;
JMP_(stg_gc_enter_1);
FE_
}

static StgWord sye_info[] = {
((W_)&sye_srt+0), 0x10005U, 0x0, 0x1f000fU
};

EI_(base_GHCziBase_zpzp_closure);
EI_(ghczmprim_GHCziTypes_ZC_con_info);
EI_(ghczmprim_GHCziTypes_ZMZN_closure);
II_(syc_info);
II_(sye_closure);
IF_(sye_entry) {
FB_
if ((W_)(((W_)Sp - 0x4U) < (W_)SpLim)) goto _czs;
Hp=Hp+6;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czs;
Hp[-5] = (W_)&syc_info;
Hp[-3] = *Sp;
Hp[-2] = (W_)&ghczmprim_GHCziTypes_ZC_con_info;
Hp[-1] = (W_)&stg_CHARLIKE_closure+73;
*Hp = (W_)&ghczmprim_GHCziTypes_ZMZN_closure+1;
R1.w = (W_)&base_GHCziBase_zpzp_closure;
*Sp = (W_)Hp-20;
Sp[-1] = (W_)Hp-6;
Sp=Sp-1;
JMP_((W_)&stg_ap_pp_fast);
_czs:
HpAlloc = 0x18U;
R1.w = (W_)&sye_closure;
JMP_(stg_gc_fun);
FE_
}
EI_(base_SystemziIO_interact_closure);
II_(sye_closure);
static StgWord Main_main_srt[] = {
(W_)&base_SystemziIO_interact_closure, (W_)&sye_closure
};

EI_(Main_main_info);
StgWord Main_main_closure[] = {
(W_)&Main_main_info, 0x0, 0x0, 0x0
};

StgWord Main_main_info[] = {
((W_)&Main_main_srt+0), 0x0, 0x30016U
};

EI_(base_SystemziIO_interact_closure);
II_(sye_closure);
FN_(Main_main_entry) {
FB_
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _czC;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czC;
Hp[-1] = (W_)&stg_CAF_BLACKHOLE_info;
;EI_(newCAF);
((void (*)(void *))(W_)&newCAF)((void *)R1.w);
R1.p[1] = (W_)Hp-4;
*R1.p = (W_)&stg_IND_STATIC_info;
Sp[-2] = (W_)&stg_upd_frame_info;
Sp[-1] = (W_)Hp-4;
R1.w = (W_)&base_SystemziIO_interact_closure;
Sp[-3] = (W_)&sye_closure+1;
Sp=Sp-3;
JMP_((W_)&stg_ap_p_fast);
_czC:
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
if ((W_)(((W_)Sp - 0xcU) < (W_)SpLim)) goto _czM;
Hp=Hp+2;
if ((W_)((W_)Hp > (W_)HpLim)) goto _czM;
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
_czM:
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
if ((W_)(0x0 != (*((P_)(W_)&_module_registered)))) goto _czS;
goto _czU;
_czS:
Sp=Sp+1;
JMP_(Sp[-1]);
_czU:
*((P_)(W_)&_module_registered) = 0x1U;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_Prelude_;
Sp=Sp-1;
*Sp = (W_)&__stginit_base_GHCziTopHandler_;
goto _czS;
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
