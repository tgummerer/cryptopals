package challenge_23

import "testing"

func TestUntemper(t *testing.T) {
	tempered := uint32(3310923448)
	untemperedOrig := uint32(397950697)

	untempered := untemper(tempered)
	if untemperedOrig != untempered {
		t.Log("tempered and untempered don't match: ", untemperedOrig, untempered)
		t.Fail()
	}
}
