module term

// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
   // TODO Find a way to get proper width & height of the terminal
   // on a Javascript environment
   return default_columns_size, default_rows_size
}
