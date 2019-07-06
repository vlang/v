/*
https://benchmarksgame-team.pages.debian.net/benchmarksgame/performance/spectralnorm.html
Added: Pradeep Verghese
Benchmarks: 
Used v -prod spectral.v
Command: time ./spectral 5500
Output: 1.274224153

Time: 11.67s user 0.02s system 99% cpu 11.721 total
*/

module main
import math
import os


fn evala(i, j int) int {
    return ((i+j)*(i+j+1)/2 + i + 1)
}

fn (v mut []f64) times(u []f64) {
    for i := 0; i < v.len; i++ {
        mut a := f64(0)
        for j :=0; j< u.len; j++ {
            a += u[j] /f64(evala(i,j))
        }
        v[i] = a
    }
}

fn (v mut []f64) times_trans(u []f64) {
    for i := 0; i< v.len; i++ {
        mut a := f64(0)
        for j :=0; j< u.len; j++ {
            a += u[j] / f64(evala(j,i))
        }
        v[i] = a
    }
}

fn (v mut []f64) a_times_transp(u []f64) {
    mut x := [f64(0); u.len]
    x.times(u)
    v.times_trans(x)
} 

fn main() {

    args := os.args
    mut n := int(0)

    if args.len == 2 {
        n = args[1].int()
    }
    else {
        n = 0 
    }
    mut u := [f64(1.0);n]
    mut v := [f64(1.0);n]

    for i := 0; i< 10; i++ {
        v.a_times_transp(u)
        u.a_times_transp(v)
    }

    mut vbv := f64(0)
    mut vv  := f64(0)

    for i :=0; i< n; i++ {
        vbv += u[i] * v[i]
        vv += v[i] * v[i]
    }
    ans := math.sqrt(vbv/vv)

    println('${ans:0.9f}')


}
