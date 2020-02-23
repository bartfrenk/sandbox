package main

import (
	"fmt"
	"github.com/bartfrenk/trading/bla"
	"net/http"
)

type Trade struct {
	EventType          string `json:"e"`
	EventTime          uint64 `json:"E"`
	Symbol             string `json:"s"`
	Price              string `json:"p"`
	Quantity           string `json:"q"`
	BuyerOrderId       uint64 `json:"b"`
	SellerOrderId      uint64 `json:"a"`
	TradeTime          uint64 `json:"T"`
	BuyerIsMarketMaker bool   `json:"m"`
	Ignored            bool   `json:"M"`
}

func main() {
	http.Get("www.google.com")
	bla.Baz("bla")
	fmt.Println("Hello world.")
}
