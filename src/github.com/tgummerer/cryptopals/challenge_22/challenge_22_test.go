package challenge_22

import "testing"

func TestRngCracking(t *testing.T) {
	real, cracked := CrackRngSeed()

	if real != cracked {
		t.Log("Cracked seed != real seed", real, cracked)
		t.Fail()
	}
}
