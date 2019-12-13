module sdl

pub union C.SDL_Event {
pub:
	@type u32
	display C.SDL_DisplayEvent
	window C.SDL_WindowEvent
	key C.SDL_KeyboardEvent
	edit C.SDL_TextEditingEvent
	text C.SDL_TextInputEvent
	motion C.SDL_MouseMotionEvent
	button C.SDL_MouseButtonEvent
	wheel C.SDL_MouseWheelEvent
	jaxis C.SDL_JoyAxisEvent
	jball C.SDL_JoyBallEvent
	jhat C.SDL_JoyHatEvent
	jbutton C.SDL_JoyButtonEvent
	jdevice C.SDL_JoyDeviceEvent
	caxis C.SDL_ControllerAxisEvent
	cbutton C.SDL_ControllerButtonEvent
	cdevice C.SDL_ControllerDeviceEvent
	adevice C.SDL_AudioDeviceEvent
	sensor C.SDL_SensorEvent
	quit C.SDL_QuitEvent
	user C.SDL_UserEvent
	tfinger C.SDL_TouchFingerEvent
	mgesture C.SDL_MultiGestureEvent
	drop C.SDL_DropEvent

	_pad56 [56]byte
}

// individual event declarations
pub struct C.SDL_DisplayEvent {
    @type u32        /**< ::SDL_DISPLAYEVENT */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    display u32     /**< The associated display index */
    event byte        /**< ::SDL_DisplayEventID */
    padding1 byte
    padding2 byte
    padding3 byte
    data1 int       /**< event dependent data */
}

pub struct C.SDL_WindowEvent {
pub:
	@type u32           /**< ::SDL_WINDOWEVENT */
	timestamp u32       /**< In milliseconds, populated using SDL_GetTicks() */
	windowID u32        /**< The associated window */
	event byte          /**< ::SDL_WindowEventID */
	padding1 byte
	padding2 byte
	padding3 byte
	data1 int
	data2 int
}

pub struct C.SDL_KeyboardEvent {
pub:
	@type u32   	/**< ::SDL_KEYDOWN or ::SDL_KEYUP */
	timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
	windowID u32   /**< The window with keyboard focus, if any */
	state byte  	/**< ::SDL_PRESSED or ::SDL_RELEASED */
	repeat byte     /**< Non-zero if this is a key repeat */
	padding2 byte
	padding3 byte
	keysym Keysym
}

pub struct C.SDL_TextEditingEvent {
pub:
    @type u32                                /**< ::SDL_TEXTEDITING */
    timestamp u32                           /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32                            /**< The window with keyboard focus, if any */
    text [32]byte  /**< The editing text */
    start int                               /**< The start cursor of selected editing text */
    length int                              /**< The length of selected editing text */
}

pub struct C.SDL_TextInputEvent {
pub:
    @type u32                              /**< ::SDL_TEXTINPUT */
    timestamp u32                         /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32                          /**< The window with keyboard focus, if any */
    text [32]byte  /**< The input text */
}

pub struct C.SDL_MouseMotionEvent {
pub:
    @type u32        /**< ::SDL_MOUSEMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    state u32       /**< The current button state */
    x int           /**< X coordinate, relative to window */
    y int           /**< Y coordinate, relative to window */
    xrel int        /**< The relative motion in the X direction */
    yrel int        /**< The relative motion in the Y direction */
}

pub struct C.SDL_MouseButtonEvent {
pub:
    @type u32        /**< ::SDL_MOUSEBUTTONDOWN or ::SDL_MOUSEBUTTONUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    button byte       /**< The mouse button index */
    state byte        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    clicks byte       /**< 1 for single-click, 2 for double-click, etc. */
    padding1 byte
    x int           /**< X coordinate, relative to window */
    y int           /**< Y coordinate, relative to window */
}

pub struct C.SDL_MouseWheelEvent {
pub:
    @type u32        /**< ::SDL_MOUSEWHEEL */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32    /**< The window with mouse focus, if any */
    which u32       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    x int           /**< The amount scrolled horizontally, positive to the right and negative to the left */
    y int           /**< The amount scrolled vertically, positive away from the user and negative toward the user */
    direction u32   /**< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back */
}

pub struct C.SDL_JoyAxisEvent {
pub:
    @type u32        /**< ::SDL_JOYAXISMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    axis byte         /**< The joystick axis index */
    padding1 byte
    padding2 byte
    padding3 byte
    value i16       /**< The axis value (range: -32768 to 32767) */
    padding4 u16
}

pub struct C.SDL_JoyBallEvent {
pub:
    @type u32        /**< ::SDL_JOYBALLMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    ball byte         /**< The joystick trackball index */
    padding1 byte
    padding2 byte
    padding3 byte
    xrel i16        /**< The relative motion in the X direction */
    yrel i16        /**< The relative motion in the Y direction */
}

pub struct C.SDL_JoyHatEvent {
pub:
	@type u32       /**< SDL_JOYHATMOTION */
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

pub struct C.SDL_JoyButtonEvent {
pub:
	@type u32 		/**< SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP */
	timestamp u32
	which int 		/**< The joystick device index */
	button byte		/**< The joystick button index */
	state byte		/**< SDL_PRESSED or SDL_RELEASED */
    padding1 byte
    padding2 byte
}

pub struct C.SDL_JoyDeviceEvent {
pub:
    @type u32        /**< ::SDL_JOYDEVICEADDED or ::SDL_JOYDEVICEREMOVED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The joystick device index for the ADDED event, instance id for the REMOVED event */
}

pub struct C.SDL_ControllerAxisEvent {
pub:
    @type u32        /**< ::SDL_CONTROLLERAXISMOTION */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    axis byte         /**< The controller axis (SDL_GameControllerAxis) */
    padding1 byte
    padding2 byte
    padding3 byte
    value i16       /**< The axis value (range: -32768 to 32767) */
    padding4 u16
}

pub struct C.SDL_ControllerButtonEvent {
pub:
    @type u32        /**< ::SDL_CONTROLLERBUTTONDOWN or ::SDL_CONTROLLERBUTTONUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int /**< The joystick instance id */
    button byte       /**< The controller button (SDL_GameControllerButton) */
    state byte        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    padding1 byte
    padding2 byte
}

pub struct C.SDL_ControllerDeviceEvent {
pub:
    @type u32        /**< ::SDL_CONTROLLERDEVICEADDED, ::SDL_CONTROLLERDEVICEREMOVED, or ::SDL_CONTROLLERDEVICEREMAPPED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event */
}

pub struct C.SDL_AudioDeviceEvent {
pub:
    @type u32        /**< ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which u32       /**< The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event */
    iscapture byte    /**< zero if an output device, non-zero if a capture device. */
    padding1 byte
    padding2 byte
    padding3 byte
}

pub struct C.SDL_TouchFingerEvent {
pub:
    @type u32        /**< ::SDL_FINGERMOTION or ::SDL_FINGERDOWN or ::SDL_FINGERUP */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    touchId i64 /**< The touch device id */
    fingerId i64
    x f32            /**< Normalized in the range 0...1 */
    y f32            /**< Normalized in the range 0...1 */
    dx f32           /**< Normalized in the range -1...1 */
    dy f32           /**< Normalized in the range -1...1 */
    pressure f32     /**< Normalized in the range 0...1 */
}

pub struct C.SDL_MultiGestureEvent {
pub:
    @type u32        /**< ::SDL_MULTIGESTURE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    touchId i64 /**< The touch device id */
    dTheta f32
    dDist f32
    x f32
    y f32
    numFingers u16
    padding u16
}

pub struct C.SDL_DropEvent {
pub:
    @type u32        /**< ::SDL_DROPBEGIN or ::SDL_DROPFILE or ::SDL_DROPTEXT or ::SDL_DROPCOMPLETE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    file byteptr         /**< The file name, which should be freed with SDL_free(), is NULL on begin/complete */
    windowID u32    /**< The window that was dropped on, if any */
}

pub struct C.SDL_SensorEvent {
pub:
    @type u32        /**< ::SDL_SENSORUPDATE */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    which int       /**< The instance ID of the sensor */
    data [6]f32      /**< Up to 6 values from the sensor - additional values can be queried using SDL_SensorGetData() */
}

pub struct C.SDL_QuitEvent {
pub:
	@type u32 /**< SDL_QUIT */
	timestamp u32
}

pub struct C.SDL_UserEvent {
pub:
    @type u32        /**< ::SDL_USEREVENT through ::SDL_LASTEVENT-1 */
    timestamp u32   /**< In milliseconds, populated using SDL_GetTicks() */
    windowID u32    /**< The associated window if any */
    code int        /**< User defined event code */
    data1 voidptr        /**< User defined data pointer */
    data2 voidptr        /**< User defined data pointer */
}
