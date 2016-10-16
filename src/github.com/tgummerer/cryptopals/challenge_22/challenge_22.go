package challenge_22

import (
	"github.com/tgummerer/cryptopals/challenge_21"
	"time"
)

func CrackRngSeed() (uint32, uint32) {
	var rng challenge_21.MT19937

	rng.Init(uint32(time.Now().Unix()))

	time.Sleep(time.Duration(rng.ExtractU32()%30) * time.Second)

	rngSeed := uint32(time.Now().Unix())
	rng.Init(rngSeed)

	extracted := rng.ExtractU32()

	time.Sleep(time.Duration(rng.ExtractU32()%30) * time.Second)

	for i := uint32(time.Now().Unix()) - 2000; i < uint32(time.Now().Unix()); i++ {
		rng.Init(i)
		newExtraction := rng.ExtractU32()
		if newExtraction == extracted {
			return rngSeed, i
		}
	}

	return rngSeed, 0
}
