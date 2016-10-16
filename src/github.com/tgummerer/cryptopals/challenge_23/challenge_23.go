package challenge_23

import (
	"github.com/tgummerer/cryptopals/challenge_21"
)

func unshiftRight(tempered uint32, shift uint32) uint32 {
	result := uint32(0)
	for i := uint32(0); i*shift < 32+shift; i++ {
		partMask := uint32(0xFFFFFFFF<<(32-shift)) >> (i * shift)
		part := tempered & partMask

		tempered ^= (part >> shift)
		result |= part
	}
	return result
}

func unshiftLeft(tempered uint32, shift uint32, mask uint32) uint32 {
	result := uint32(0)
	for i := uint32(0); i*shift < 32; i++ {
		partMask := (0xFFFFFFFF >> (32 - shift)) << (i * shift)
		part := tempered & uint32(partMask)

		tempered ^= (part << shift) & mask
		result |= part
	}
	return result
}

func untemper(tempered uint32) uint32 {
	tempered = unshiftRight(tempered, 18)
	tempered = unshiftLeft(tempered, 15, 0xEFC60000)
	tempered = unshiftLeft(tempered, 7, 0x9D2C5680)
	tempered = unshiftRight(tempered, 11)
	return tempered
}

func MT19937Clone(m challenge_21.MT19937) challenge_21.MT19937 {
	var cloned challenge_21.MT19937
	cloned.Init(0)

	for i := 0; i < 624; i++ {
		cloned.Mt[i] = untemper(m.ExtractU32())
	}
	cloned.Index = 0
	return cloned
}
