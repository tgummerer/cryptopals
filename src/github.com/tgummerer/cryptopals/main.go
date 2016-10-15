package main

import (
	"github.com/tgummerer/cryptopals/challenge_21"
	"github.com/tgummerer/cryptopals/challenge_22"
	"fmt"
)

func main() {
	var twister challenge_21.MT19937

	twister.Init(20)

	fmt.Println("challenge 21, two pseudo random numbers")
	fmt.Println(twister.ExtractU32())
	fmt.Println(twister.ExtractU32())

	fmt.Println("challenge 22, crack the seed")
	real, cracked := challenge_22.CrackRngSeed()
	fmt.Printf("original: %d cracked: %d\n", real, cracked)
}
