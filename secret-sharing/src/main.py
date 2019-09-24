import re
from subprocess import PIPE, STDOUT, run

from cryptography.fernet import Fernet


def encrypt(message):
    key = Fernet.generate_key()
    cipher = Fernet(key)
    return {"key": key, "ciphertext": cipher.encrypt(message)}


def decrypt(key, ciphertext):
    cipher = Fernet(key)
    return cipher.decrypt(ciphertext)


def read_message(path):
    with open(path, "rb") as h:
        return h.read()


def share_secret(t, n, secret):
    p = run(
        ["ssss-split", f"-t {t}", f"-n {n}", f"-w share", "-x"],
        stdout=PIPE,
        stderr=STDOUT,
        input=secret,
        check=True,
        encoding="ascii",
    )

    print(p.stdout)
    shares = []
    for line in p.stdout.strip("\n").split("\n")[-n:]:
        m = re.match(r" (share-\d-[a-f0-9]*)", line)
        if m:
            shares.append(m.group(1))
    return shares


def recover_secret(shares):
    p = run(
        ["ssss-combine", f"-t {len(shares)}", "-x"],
        stdout=PIPE,
        stderr=STDOUT,
        input="\n".join(shares),
        check=False,
        encoding="ascii",
    )
    print(p.stdout)
    for line in p.stdout.split("\n"):
        m = re.match("Resulting secret: (.+)$", line)
        if m:
            return m.group(1)
    return None


def main():
    t = 2
    n = 3
    message = read_message("resources/secret.key")
    m = encrypt(message)

    top_shares = share_secret(2, 2, m["key"].hex())

    print("top_shares:", top_shares)
    print(len(top_shares[1][8:]) / 2)

    shares = share_secret(t, n, top_shares[1][8:])
    print("shares:", shares)
    rec_leaf_share = recover_secret(shares[:t])
    print("rec_leaf_share:", rec_leaf_share)
    rec_key = recover_secret([f"share-2-{rec_leaf_share}", top_shares[0]])
    print("rec_key:", rec_key)

    decrypted = decrypt(bytes.fromhex(rec_key), m["ciphertext"])
    print(decrypted.decode("utf-8"))
    print("Correctly decoded:", decrypted == message)
    print()

    print("keep: ", top_shares[0])
    print("distribute", shares)
    print("publish", m["ciphertext"])


if __name__ == "__main__":
    main()
