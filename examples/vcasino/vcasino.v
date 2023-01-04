import rand
import os

const (
	help_text = ' Usage:\t./VCasino\n
 Description:\n  VCasino is a little game only made to learn V.\n'
	g_desc = "  The object of Roulette is to pick the number where the spinning ball will land on the wheel.
   If your number is the good one, you'll get your bet x3.
   If your number is the same color as the ball one, you'll get your bet /2.
   Otherwise, you will lose your bet.\n"
	odd  = 'red'
	even = 'black'
)

struct Options {
	long_opt  string
	short_opt string
}

fn display_help() {
	println(help_text + g_desc)
}

fn option_parser() bool {
	help := Options{'--help', '-h'}
	for i in 0 .. os.args.len {
		if os.args[i] == help.long_opt || os.args[i] == help.short_opt {
			display_help()
			return true
		}
	}
	return false
}

fn str_is_nbr(s string) bool {
	for i in 0 .. s.len {
		if !s[i].is_digit() {
			return false
		}
	}
	return true
}

fn get_bet_nbr() int {
	mut bet_nbr := -1
	for bet_nbr < 0 || bet_nbr > 49 {
		println('Reminder: odd numbers are red and even are black.')
		println('Type the number you want to bet on (between 0 and 49):')
		line := os.get_line().trim_space()
		if line.len < 1 {
			println('error: empty line.')
			continue
		}
		if !str_is_nbr(line) {
			println('error: ${line} is not a number.')
			continue
		}
		bet_nbr = line.int()
		if bet_nbr < 0 || bet_nbr > 49 {
			println('error: ${line} is not between 0 and 49.')
			bet_nbr = -1
			continue
		}
	}
	return bet_nbr
}

fn get_bet(money int) int {
	mut bet := -1
	for bet <= 0 || bet > money {
		println('You have ${money} V. Type in the amount of your bet:')
		line := os.get_line().trim_space()
		if line.len < 1 {
			println('error: empty line.')
			continue
		}
		if !str_is_nbr(line) {
			println('error: ${line} is not a number.')
			continue
		}
		bet = line.int()
		if bet <= 0 {
			println('error: ${line} is not higher than 1.')
			continue
		} else if bet > money {
			println('error: ${line} is more money than you have.')
		}
	}
	return bet
}

fn run_wheel(bet_nbr int, _bet int) int {
	mut bet := _bet
	winning_nbr := rand.intn(50) or { 0 }
	print('Roulette Wheel spinning... and stops on the number ${winning_nbr} which is a ')
	if winning_nbr % 2 == 1 {
		println(odd)
	} else {
		println(even)
	}
	if winning_nbr == bet_nbr {
		bet *= 3
		println('Congratulations! You get ${bet} V!')
	} else if winning_nbr % 2 == bet_nbr % 2 {
		bet /= 2
		println('You bet the right color. You get ${bet} V!')
	} else {
		println('Sorry buddy. You lost ${bet} V!')
		bet *= -1
	}
	return bet
}

fn is_broke(money int) bool {
	if money <= 0 {
		println("You're broke, the game is over..")
		return false
	}
	quit := Options{'yes', 'y'}
	println('You have ${money} V. Do you want to quit the casino with your winnings? (y/n)')
	line := os.get_line().trim_space().to_lower()
	if line == quit.long_opt || line == quit.short_opt {
		return false
	}
	return true
}

fn game_loop() {
	mut can_play := true
	mut money := 1000
	println(g_desc)
	println('You start the game with ${money} V.\n')
	for can_play {
		bet_nbr := get_bet_nbr()
		bet := get_bet(money)
		money += run_wheel(bet_nbr, bet)
		can_play = is_broke(money)
	}
}

fn main() {
	if os.args.len >= 2 && option_parser() {
		return
	}
	game_loop()
}
