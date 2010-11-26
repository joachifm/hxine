/* Support for conditional compilation depending on xine-lib version

   Example:

   #if XINE_CHECK_VERSION (1, 1, 20)
   announce = "xine-lib version is at least 1.1.20"
   #else
   announce = "xine-lib version is less than 1.1.20"
   #endif
*/

#include <xine.h>

/* Check that xine-lib version is at least MINOR.MAJOR.MICRO */
#define XINE_CHECK_VERSION(major, minor, sub) \
    (XINE_MAJOR_VERSION > (major) || \
    (XINE_MAJOR_VERSION == (major) && XINE_MINOR_VERSION > (minor)) || \
    (XINE_MAJOR_VERSION == (major) && XINE_MINOR_VERSION == (minor) && \
    XINE_SUB_VERSION >= (sub)))
