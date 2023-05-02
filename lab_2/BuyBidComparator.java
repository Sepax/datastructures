
import java.util.Comparator;

public class BuyBidComparator implements Comparator<Bid> {
    @Override
    public int compare(Bid firstBid, Bid secondBid) {
        return Integer.compare(secondBid.bid, firstBid.bid);
    }
}
