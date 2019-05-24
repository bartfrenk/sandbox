import asyncio
import sys


class Test:
    def __init__(self, number):
        self.number = number

    async def run(self):
        print("The magic number is...", end=" ")
        sys.stdout.flush()
        await asyncio.sleep(1)
        print(self.number)


async def main():
    print("Hello")
    await asyncio.sleep(1)
    print("... World!")


if __name__ == "__main__":
    test = Test(5)
    asyncio.run(test.run())
