import rand
import os

const (HelpText = ' Usage:\t./VCasino\n
 Description:\n  VCasino is a little game only made to learn V.\n')
const (GDesc = '  The object of Roulette is to pick the number where the spinning ball will land on the wheel.
   If your number is the good one, you\'ll get your bet x3.
   If your number is the same color as the ball one, you\'ll get your bet /2.
   Otherwise, you will lose your bet.\n')
const (Odd = 'Red')
const (Even = 'Black')

struct options {
    long_opt string
    short_opt string
}

fn display_help() {
    println(HelpText + GDesc)
}

fn option_parser() bool {
    help := options{'--help', '-h'}
    for i := 0; i < os.args.len; i++ {
        if ((!compare_strings(os.args[i], help.long_opt)) || (!compare_strings(os.args[i], help.short_opt))) {
            display_help()
            return true
        }
    }
    return false
}

fn str_is_nbr(s string) bool {
	for i := 0; i < s.len; i++ {
        if (!s[i].is_digit()) {
            return false
        }
    }
    return true
}

fn get_bet_nbr() int {
    mut bet_nbr := -1
    for bet_nbr < 0 || bet_nbr > 49 {
        println('Reminder: Odd numbers are red and even are black.')
        println('Type the number you want to bet on (between 0 and 49):')
		line := os.get_line().trim_space()
        if line.len < 1 {
            println('error: empty line.')
            continue
        }
        if !str_is_nbr(line) {
            println('error: $line is not a number.')
            continue
        }
        bet_nbr = line.to_i()
        if bet_nbr < 0 || bet_nbr > 49 {
            println('error: $line is not between 0 and 49.')
            bet_nbr = -1
            continue
        }
    }
    return bet_nbr
}

fn get_bet(money int) int {
    mut bet := -1
    for bet <= 0 || bet > money {
        println('You\'ve $money V. Type in the amount of your bet:')
		line := os.get_line().trim_space()
        if line.len < 1 {
            println('error: empty line.')
            continue
        }
        if !str_is_nbr(line) {
            println('error: $line is not a number.')
            continue
        }
        bet = line.to_i()
        if bet <= 0 {
            println('error: $line is not heigher than 1.')
            continue
        } else if bet > money {
            println('error: $line is not heigher than your money.')
        }
    }
    return bet
}

fn run_wheel(bet_nbr int, bet int) int {
    rand.seed()
    winning_nbr := rand.next(50)
    print('Roulette Wheel spinning... and stops on the number $winning_nbr which is a ')
    if winning_nbr % 2 == 1 {
        println(Odd)
    } else {
        println(Even)
    }
    if winning_nbr == bet_nbr {
        bet *= 3
        println('Congratulations! You get $bet V!')
    } else if winning_nbr % 2 == bet_nbr % 2 {
        bet /= 2
        println('You bet the right color. You get $bet V!')
    } else {
        println('Sorry buddy. You lost $bet V!')
        bet *= -1
    }
    return bet
}

fn is_broke(money int) bool {
    if money <= 0 {
        println('You\'broke, the game is over..')
        return false
    } else {
        quit := options{'yes', 'y'}
        println('You\'ve $money V. Do you want to quit the casino with your winnings? (y/n)')
   	    line := os.get_line().trim_space().to_lower()
        if ((!compare_strings(line, quit.long_opt)) || (!compare_strings(line, quit.short_opt))) {
            return false
        }
    }
    return true
}

fn game_loop() {
    mut can_play := true
    mut money := 1000

    println(GDesc)
    println('You start the game with $money V.\n')
    for can_play {
        bet_nbr := get_bet_nbr()
        bet := get_bet(money)
        money += run_wheel(bet_nbr, bet)
        can_play = is_broke(money)
    }
}

fn main() {
    if os.args.len >= 2 {
        if option_parser() {
            return
        }
    }
    game_loop()
}