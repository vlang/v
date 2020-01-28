module term

pub fn get_terminal_size() (int, int) { 
   // TODO: find a way to get proper width&height of the terminal on windows, probably using Winapis
   return 80, 25  
}
