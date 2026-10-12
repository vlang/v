#include <stddef.h>
#include "appkit_backend_helpers.h"
#include "../../../thirdparty/sokol/sokol_gfx.h"
int64_t v_appkit_native_layout(int32_t type, int32_t member) {
  switch (type) {
  case 0: switch (member) {
    case -1: return sizeof(VMultiwindowNativePrimitive);
    case 0: return offsetof(VMultiwindowNativePrimitive, valid_mask);
    case 1: return offsetof(VMultiwindowNativePrimitive, return_value);
    case 2: return offsetof(VMultiwindowNativePrimitive, handle);
    case 3: return offsetof(VMultiwindowNativePrimitive, egl_error);
    case 4: return offsetof(VMultiwindowNativePrimitive, native_errno);
    case 5: return offsetof(VMultiwindowNativePrimitive, wayland_display_error);
    case 6: return offsetof(VMultiwindowNativePrimitive, dxgi_removal_reason);
    case 7: return offsetof(VMultiwindowNativePrimitive, observed_count);
    case 8: return offsetof(VMultiwindowNativePrimitive, observed_flags);
    case 9: return offsetof(VMultiwindowNativePrimitive, selected_value);
    case 10: return offsetof(VMultiwindowNativePrimitive, object_identity_0);
    case 11: return offsetof(VMultiwindowNativePrimitive, object_identity_1);
    case 12: return offsetof(VMultiwindowNativePrimitive, object_identity_2);
  } break;
  case 1: switch (member) {
    case -1: return sizeof(VMultiwindowAppKitQueuedEvent);
    case 0: return offsetof(VMultiwindowAppKitQueuedEvent, sequence);
    case 1: return offsetof(VMultiwindowAppKitQueuedEvent, event_kind);
    case 2: return offsetof(VMultiwindowAppKitQueuedEvent, lifecycle_kind);
    case 3: return offsetof(VMultiwindowAppKitQueuedEvent, input_kind);
    case 4: return offsetof(VMultiwindowAppKitQueuedEvent, key_code);
    case 5: return offsetof(VMultiwindowAppKitQueuedEvent, char_code);
    case 6: return offsetof(VMultiwindowAppKitQueuedEvent, key_repeat);
    case 7: return offsetof(VMultiwindowAppKitQueuedEvent, modifiers);
    case 8: return offsetof(VMultiwindowAppKitQueuedEvent, mouse_button);
    case 9: return offsetof(VMultiwindowAppKitQueuedEvent, mouse_x);
    case 10: return offsetof(VMultiwindowAppKitQueuedEvent, mouse_y);
    case 11: return offsetof(VMultiwindowAppKitQueuedEvent, mouse_dx);
    case 12: return offsetof(VMultiwindowAppKitQueuedEvent, mouse_dy);
    case 13: return offsetof(VMultiwindowAppKitQueuedEvent, scroll_x);
    case 14: return offsetof(VMultiwindowAppKitQueuedEvent, scroll_y);
    case 15: return offsetof(VMultiwindowAppKitQueuedEvent, window_width);
    case 16: return offsetof(VMultiwindowAppKitQueuedEvent, window_height);
    case 17: return offsetof(VMultiwindowAppKitQueuedEvent, framebuffer_width);
    case 18: return offsetof(VMultiwindowAppKitQueuedEvent, framebuffer_height);
    case 19: return offsetof(VMultiwindowAppKitQueuedEvent, service_snapshot_valid);
    case 20: return offsetof(VMultiwindowAppKitQueuedEvent, service_kind);
    case 21: return offsetof(VMultiwindowAppKitQueuedEvent, service_operation);
    case 22: return offsetof(VMultiwindowAppKitQueuedEvent, service_mapping);
    case 23: return offsetof(VMultiwindowAppKitQueuedEvent, service_visibility);
    case 24: return offsetof(VMultiwindowAppKitQueuedEvent, service_active);
    case 25: return offsetof(VMultiwindowAppKitQueuedEvent, service_focused);
    case 26: return offsetof(VMultiwindowAppKitQueuedEvent, service_minimized);
    case 27: return offsetof(VMultiwindowAppKitQueuedEvent, service_maximized);
    case 28: return offsetof(VMultiwindowAppKitQueuedEvent, service_fullscreen);
    case 29: return offsetof(VMultiwindowAppKitQueuedEvent, service_mouse_locked);
    case 30: return offsetof(VMultiwindowAppKitQueuedEvent, service_position_known);
    case 31: return offsetof(VMultiwindowAppKitQueuedEvent, service_x);
    case 32: return offsetof(VMultiwindowAppKitQueuedEvent, service_y);
    case 33: return offsetof(VMultiwindowAppKitQueuedEvent, service_monitor_native_id);
    case 34: return offsetof(VMultiwindowAppKitQueuedEvent, service_scale);
    case 35: return offsetof(VMultiwindowAppKitQueuedEvent, dropped_file_count);
    case 36: return offsetof(VMultiwindowAppKitQueuedEvent, dropped_files);
    case 37: return offsetof(VMultiwindowAppKitQueuedEvent, touch_count);
    case 38: return offsetof(VMultiwindowAppKitQueuedEvent, touch_ids);
    case 39: return offsetof(VMultiwindowAppKitQueuedEvent, touch_x);
    case 40: return offsetof(VMultiwindowAppKitQueuedEvent, touch_y);
    case 41: return offsetof(VMultiwindowAppKitQueuedEvent, touch_changed);
  } break;
  case 2: switch (member) {
    case -1: return sizeof(sg_mtl_image_info);
    case 0: return offsetof(sg_mtl_image_info, tex);
    case 1: return offsetof(sg_mtl_image_info, active_slot);
  } break;
  }
  return -1;
}
