import v.reflection

const provide_key = 'provide'

type InjectCb = fn () !

pub interface Object {}

pub struct Service {
	typ         int      @[required]
	inject      InjectCb @[required]
	name        string   @[required]
	originalptr voidptr  @[required]
mut:
	instance &Object @[required]
}

pub struct Module {
mut:
	services map[int]Service = map[int]Service{}
}

pub fn (mut self Module) inject_to_object[T](mut new_service T) ! {
	$for field in T.fields {
		mut service := self.get_service_from_field(field, T.name)!
		$if field.indirections > 0 {
			if mut service is Service {
				if service.typ != field.typ {
					return error("Type of property '${field.name}' in '${T.name}' must be ${service.name} as Reference")
				}
				unsafe {
					new_service.$(field.name) = service.originalptr
				}
			}
		} $else {
			if service is Service {
				println('Warning: field ${field.name} must be a reference to enable autoinject')
			}
		}
	}
}

pub fn (mut self Module) register[T]() &T {
	mut new_service := &T{}
	typ_idx := typeof[T]().idx
	typ := reflection.get_type(typ_idx) or { panic('Type not found ${T.name}') }

	if typ_idx in self.services {
		panic('Type ${typ.name} has been already registered')
	}

	self.services[typ_idx] = Service{
		name:        T.name
		typ:         typ_idx
		instance:    new_service
		originalptr: new_service
		inject:      fn [mut self, mut new_service] [T]() ! {
			self.inject_to_object[T](mut new_service)!
		}
	}
	return new_service
}

fn get_key(attrs []string) string {
	return attrs[0] or { 'key' }
}

type ServiceOrNone = Service | bool

fn (mut self Module) get_service_from_field(field FieldData, t_name string) !ServiceOrNone {
	if 'inject' in field.attrs {
		return self.get_service(field.typ) or {
			return error('Invalid injection type for field ${field.name} in ${t_name}')
		}
	}
	return false
}

fn (mut self Module) get_service(service_idx int) !Service {
	return self.services[service_idx] or {
		service_info := reflection.get_type(service_idx) or { return err }
		return error('Service with name ${service_info.name} not available, see available: ${self.services.keys()}')
	}
}

struct Something {
	count int
}

struct SomethingDifferent {
	text string
}

fn test_main() {
	mut mod := Module{}
	mod.register[SomethingDifferent]()
	mod.register[Something]()
	assert true
}
