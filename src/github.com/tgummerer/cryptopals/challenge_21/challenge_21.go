package challenge_21

type MT19937 struct {
	n uint32
	m uint32
	r uint32
	a uint32

	f uint32

	u uint32

	s uint32
	b uint32

	t uint32
	c uint32

	l uint32

	maskLower uint32
	maskUpper uint32

	Mt    []uint32
	Index uint32
}

func (m *MT19937) Init(seed uint32) {
	m.n = 624
	m.m = 397
	m.r = 31
	m.a = 0x9908B0DF

	m.f = 1812433253

	m.u = 11

	m.s = 7
	m.b = 0x9D2C5680

	m.t = 15
	m.c = 0xEFC60000

	m.l = 18

	m.maskLower = (1 << m.r) - 1
	m.maskUpper = (1 << m.r)

	m.Mt = make([]uint32, m.n)

	m.Mt[0] = seed

	var i uint32
	for i = 1; i < m.n; i++ {
		m.Mt[i] = (m.f*(m.Mt[i-1]^(m.Mt[i-1]>>30)) + 1)
	}
	m.Index = m.n
}

func (m *MT19937) twist() {
	var i uint32
	for i = 0; i < m.n; i++ {
		x := (m.Mt[i] & m.maskUpper) + (m.Mt[(i+1)%m.n] & m.maskLower)
		xa := x >> 1
		if x&0x1 > 0 {
			xa ^= m.a
		}
		m.Mt[i] = m.Mt[(i+m.m)%m.n] ^ xa
	}

	m.Index = 0
}

func (m *MT19937) ExtractU32() uint32 {
	i := m.Index

	if m.Index >= m.n {
		m.twist()
		i = m.Index
	}

	y := m.Mt[i]
	m.Index = i + 1

	y ^= m.Mt[i] >> m.u
	y ^= (y << m.s) & m.b
	y ^= (y << m.t) & m.c
	y ^= (y >> m.l)

	return y
}
