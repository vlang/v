module sdl

// this is the SDL_Event struct manually created in V. Note that the "type" field had to be renamed
// "typ" to avoid V compiler errors. We use this struct along with the actual SDL_Event struct in a
// union (EventWrapper).
pub union Event {
pub:
	typ u32
	display DisplayEvent
	window WindowEvent
	key KeyboardEvent

	edit TextEditingEvent
	text TextInputEvent
	motion MouseMotionEvent
	button MouseButtonEvent
	wheel MouseWheelEvent
	jaxis JoyAxisEvent
	jball JoyBallEvent
	jhat JoyHatEvent
	jbutton JoyButtonEvent
	jdevice JoyDeviceEvent
	caxis ControllerAxisEvent
	cbutton ControllerButtonEvent
	cdevice ControllerDeviceEvent
	adevice AudioDeviceEvent
	sensor SensorEvent
	quit QuitEvent
	user UserEvent
	tfinger TouchFingerEvent
	mgesture MultiGestureEvent
	drop DropEvent

	_pad56 [56]byte
}

pub union C.SDL_Event {}

pub union EventWrapper {
pub:
	sdl SDL_Event
	evt Event
}


// individual event declarations
pub struct DisplayEvent {
    typ u32        /**< ::SDL_DISPLAYEVENT */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    display u32     /**< The associated display index */
    event byte        /**< ::SDL_DisplayEventID */
    padding1 byte
    padding2 byte
    padding3 byte
    data1 int       /**< event dependent data */
}

pub struct WindowEvent {
pub:
	typ u32
	timestamp u32
	window_id u32
	window_event byte
	padding1 byte
	padding2 byte
	padding3 byte
	data1 int
	data2 int
}

pub struct KeyboardEvent {
pub:
	typ u32   	/**< SDL_KEYDOWN or SDL_KEYUP */
	timestamp u32
	window_id u32
	state byte  	/**< SDL_PRESSED or SDL_RELEASED */
	repeat byte
	padding2 byte
	padding3 byte
	keysym Keysym
}

pub struct TextEditingEvent {
pub:
    typ u32                                /**< ::SDL_TEXTEDITING */
    timestamp u32                           /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32                            /**< The window with keyboard focus, if any */
    text [32]byte  /**< The editing text */
    start int                               /**< The start cursor of selected editing text */
    length int                              /**< The length of selected editing text */
}

pub struct TextInputEvent {
pub:
    typ u32                              /**< ::SDL_TEXTINPUT */
    timestamp u32                         /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32                          /**< The window with keyboard focus, if any */
    text [32]byte  /**< The input text */
}

pub struct MouseMotionEvent {
pub:
    typ u32        /**< ::SDL_MOUSEMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    state u32       /**< The current button state */
    x int           /**< X coordinate, relative to window */
    y int           /**< Y coordinate, relative to window */
    xrel int        /**< The relative motion in the X direction */
    yrel int        /**< The relative motion in the Y direction */
}

pub struct MouseButtonEvent {
pub:
    typ u32        /**< ::SDL_MOUSEBUTTONDOWN or ::SDL_MOUSEBUTTONUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    button byte       /**< The mouse button index */
    state byte        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    clicks byte       /**< 1 for single-click, 2 for double-click, etc. */
    padding1 byte
    x int           /**< X coordinate, relative to window */
    y int           /**< Y coordinate, relative to window */
}

pub struct MouseWheelEvent {
pub:
    typ u32        /**< ::SDL_MOUSEWHEEL */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    x int           /**< The amount scrolled horizontally, positive to the right and negative to the left */
    y int           /**< The amount scrolled vertically, positive away from the user and negative toward the user */
    direction u32   /**< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back */
}

pub struct JoyAxisEvent {
pub:
    typ u32        /**< ::SDL_JOYAXISMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    axis byte         /**< The joystick axis index */
    padding1 byte
    padding2 byte
    padding3 byte
    value i16       /**< The axis value (range: -32768 to 32767) */
    padding4 u16
}

pub struct JoyBallEvent {
pub:
    typ u32        /**< ::SDL_JOYBALLMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    ball byte         /**< The joystick trackball index */
    padding1 byte
    padding2 byte
    padding3 byte
    xrel i16        /**< The relative motion in the X direction */
    yrel i16        /**< The relative motion in the Y direction */
}

pub struct JoyHatEvent {
pub:
	typ u32       /**< SDL_JOYHATMOTION */
	timestamp u32
	which int       /**< The joystick device index */
	hat byte        /**< The joystick hat index */
	value byte      /**< The hat position value:
						*   SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
						*   SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
						*   SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
						*  Note that zero means the POV is centered.
						*/
    padding1 byte
    padding2 byte
}

pub struct JoyButtonEvent {
pub:
	typ u32 		/**< SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP */
	timestamp u32
	which int 		/**< The joystick device index */
	button byte		/**< The joystick button index */
	state byte		/**< SDL_PRESSED or SDL_RELEASED */
    padding1 byte
    padding2 byte
}

pub struct JoyDeviceEvent {
pub:
    typ u32        /**< ::SDL_JOYDEVICEADDED or ::SDL_JOYDEVICEREMOVED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The joystick device index for the ADDED event, instance id for the REMOVED event */
}

pub struct ControllerAxisEvent {
pub:
    typ u32        /**< ::SDL_CONTROLLERAXISMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    axis byte         /**< The controller axis (SDL_GameControllerAxis) */
    padding1 byte
    padding2 byte
    padding3 byte
    value i16       /**< The axis value (range: -32768 to 32767) */
    padding4 u16
}

pub struct ControllerButtonEvent {
pub:
    typ u32        /**< ::SDL_CONTROLLERBUTTONDOWN or ::SDL_CONTROLLERBUTTONUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    button byte       /**< The controller button (SDL_GameControllerButton) */
    state byte        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    padding1 byte
    padding2 byte
}

pub struct ControllerDeviceEvent {
pub:
    typ u32        /**< ::SDL_CONTROLLERDEVICEADDED, ::SDL_CONTROLLERDEVICEREMOVED, or ::SDL_CONTROLLERDEVICEREMAPPED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event */
}

pub struct AudioDeviceEvent {
pub:
    typ u32        /**< ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which u32       /**< The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event */
    iscapture byte    /**< zero if an output device, non-zero if a capture device. */
    padding1 byte
    padding2 byte
    padding3 byte
}

pub struct TouchFingerEvent {
pub:
    typ u32        /**< ::SDL_FINGERMOTION or ::SDL_FINGERDOWN or ::SDL_FINGERUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    touch_id i64 /**< The touch device id */
    finger_id i64
    x f32            /**< Normalized in the range 0...1 */
    y f32            /**< Normalized in the range 0...1 */
    dx f32           /**< Normalized in the range -1...1 */
    dy f32           /**< Normalized in the range -1...1 */
    pressure f32     /**< Normalized in the range 0...1 */
}

pub struct MultiGestureEvent {
pub:
    typ u32        /**< ::SDL_MULTIGESTURE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    touch_id i64 /**< The touch device id */
    d_theta f32
    d_dist f32
    x f32
    y f32
    num_fingers u16
    padding u16
}

pub struct DropEvent {
pub:
    typ u32        /**< ::SDL_DROPBEGIN or ::SDL_DROPFILE or ::SDL_DROPTEXT or ::SDL_DROPCOMPLETE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    file byteptr         /**< The file name, which should be freed with SDL_free(), is NULL on begin/complete */
    window_id u32    /**< The window that was dropped on, if any */
}

pub struct SensorEvent {
pub:
    typ u32        /**< ::SDL_SENSORUPDATE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The instance ID of the sensor */
    data [6]f32      /**< Up to 6 values from the sensor - additional values can be queried using SDL_SensorGetData() */
}

pub struct QuitEvent {
pub:
	typ u32 /**< SDL_QUIT */
	timestamp u32
}

pub struct UserEvent {
pub:
    typ u32        /**< ::SDL_USEREVENT through ::SDL_LASTEVENT-1 */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    window_id u32    /**< The associated window if any */
    code int        /**< User defined event code */
    data1 voidptr        /**< User defined data pointer */
    data2 voidptr        /**< User defined data pointer */
}
