package main

import (
	"fmt"
	"github.com/tgummerer/cryptopals/challenge_21"
//	"github.com/tgummerer/cryptopals/challenge_22"
	"github.com/tgummerer/cryptopals/challenge_23"
)

func main() {
	fmt.Println("challenge 21, two pseudo random numbers")
	var twister challenge_21.MT19937
	twister.Init(20)
	fmt.Println(twister.ExtractU32())
	fmt.Println(twister.ExtractU32())

	fmt.Println("challenge 22, crack the seed")
	// real, cracked := challenge_22.CrackRngSeed()
	// fmt.Printf("original: %d cracked: %d\n", real, cracked)

	fmt.Println("challenge 23, clone rng from output")
	var mt challenge_21.MT19937
	mt.Init(12345)

	cloned := challenge_23.MT19937Clone(mt)
	mt.Init(12345)
	fmt.Printf("original: %d, cloned: %d\n", mt.ExtractU32(), cloned.ExtractU32())
}
