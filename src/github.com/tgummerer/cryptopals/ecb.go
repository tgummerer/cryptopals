package cryptopals

import (
	"crypto/aes"
)

func encryptEcb(plaintext []byte) []byte {
	cipher, _ := aes.NewCipher([]byte("YELLOW SUBMARINE"))

	bs := 16
	if len(plaintext)%bs != 0 {
		panic("Need a multiple of the ciphersize")
	}

	ciphertext := make([]byte, len(plaintext))
	for len(plaintext) > 0 {
		cipher.Encrypt(ciphertext, plaintext)
		plaintext = plaintext[bs:]
		ciphertext = ciphertext[bs:]
	}

	return ciphertext
}
