package challenge_21

import (
	"testing"
)

func TestTwister(t *testing.T) {
	var twister MT19937

	twister.Init(1)

	number1 := twister.ExtractU32()
	number2 := twister.ExtractU32()

	if number1 == number2 {
		t.Log("Numbers equal, very unlikely to be random")
		t.Fail()
	}
}
