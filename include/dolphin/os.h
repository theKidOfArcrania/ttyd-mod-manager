#ifndef _DOLPHIN_OS_H__
#define _DOLPHIN_OS_H__

#include <platform.h>

#define OS_SOUND_MODE_MONO 0
#define OS_SOUND_MODE_STEREO 1

#define OSRoundUp32B(x) (((u32) (x) + 32 - 1) & ~(32 - 1))
#define OSRoundDown32B(x) (((u32) (x)) & ~(32 - 1))

#define OS_CACHED_REGION_PREFIX 0x8000
#define OS_BASE_CACHED (OS_CACHED_REGION_PREFIX << 16)
#define OSPhysicalToCached(paddr) ((void*) ((u32) (paddr) + OS_BASE_CACHED))

#include <dolphin/os/OSAlarm.h>
#include <dolphin/os/OSAlloc.h>
#include <dolphin/os/OSArena.h>
#include <dolphin/os/OSCache.h>
#include <dolphin/os/OSContext.h>
#include <dolphin/os/OSError.h>
#include <dolphin/os/OSException.h>
#include <dolphin/os/OSExi.h>
#include <dolphin/os/OSFont.h>
#include <dolphin/os/OSInterrupt.h>
#include <dolphin/os/OSLink.h>
#include <dolphin/os/OSMemory.h>
#include <dolphin/os/OSMutex.h>
#include <dolphin/os/OSReset.h>
#include <dolphin/os/OSResetSW.h>
#include <dolphin/os/OSSerial.h>
#include <dolphin/os/OSSync.h>
#include <dolphin/os/OSThread.h>
#include <dolphin/os/OSTime.h>

#ifdef __MWERKS__
inline void OSInitFastCast()
{
  asm {
    li r3, 4;
    oris r3, r3, 4;
    mtspr 0x392, r3;
    li r3, 5;
    oris r3, r3, 5;
    mtspr 0x393, r3;
    li r3, 6;
    oris r3, r3, 6;
    mtspr 0x394, r3;
    li r3, 7;
    oris r3, r3, 7;
    mtspr 0x395, r3;
  }
}
#endif

#endif
