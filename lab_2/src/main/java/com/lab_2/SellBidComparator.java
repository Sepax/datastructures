package com.lab_2;

import java.util.Comparator;

public class SellBidComparator implements Comparator<Bid> {
    @Override
    public int compare(Bid firstBid, Bid secondBid) {
        return Integer.compare(firstBid.bid, secondBid.bid);
    }
}
