
#include "X11/extensions/Xfixes.h"

typedef u_int64_t uint64_t;

uint64_t inline_c_Clipboard_0_eadd500bfa5081ec9059b2e216662eccda03b4de() {
return (SelectionNotify);
}


void inline_c_Clipboard_1_2a7d42ad75d9032a185874e9153d7233e37bec2e(void * disp_inline_c_0, uint64_t win_inline_c_1, uint64_t selection_inline_c_2) {

     unsigned long mask = XFixesSetSelectionOwnerNotifyMask;
     XFixesSelectSelectionInput( disp_inline_c_0
                               , win_inline_c_1
                               , selection_inline_c_2
                               , mask
                               );
     return;
    
}

