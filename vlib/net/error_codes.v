module net

/* When maps will accept generic keys, it will be better to have an enum with all the error codes.
   For now, this will work. Here is a list of all the error codes : 

   -  
*/

pub fn get_error_code() string{
	err_code := error_code()
	for key, _ in ERROR_CODES{
		if ERROR_CODES[key] == err_code{
			return key
		}
	}
	return ""
}