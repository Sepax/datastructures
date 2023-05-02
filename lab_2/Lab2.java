
import java.io.*;
import java.util.*;

public class Lab2 {
	public static String pureMain(String[] commands) {
		PriorityQueue<Bid> sell_pq = new PriorityQueue<>(new SellBidComparator());
		PriorityQueue<Bid> buy_pq = new PriorityQueue<>(new BuyBidComparator());

		StringBuilder sb = new StringBuilder();

		for (int line_no = 0; line_no < commands.length; line_no++) {
			String line = commands[line_no];
			if (line.equals(""))
				continue;

			String[] parts = line.split("\\s+");
			if (parts.length != 3 && parts.length != 4)
				throw new RuntimeException("line " + line_no + ": " + parts.length + " words");
			String name = parts[0];
			if (name.charAt(0) == '\0')
				throw new RuntimeException("line " + line_no + ": invalid name");
			String action = parts[1];
			int price;
			try {
				price = Integer.parseInt(parts[2]);
			} catch (NumberFormatException e) {
				throw new RuntimeException(
						"line " + line_no + ": invalid price");
			}

			Bid bid = new Bid(name, price);

			if (action.equals("K")) {
				buy_pq.add(bid);
			} else if (action.equals("S")) {
				sell_pq.add(bid);
			} else if (action.equals("NK")) {
				Bid newBid = new Bid(name, Integer.parseInt(parts[3]));
				buy_pq.update(bid, newBid);
			} else if (action.equals("NS")) {
				Bid newBid = new Bid(name, Integer.parseInt(parts[3]));
				sell_pq.update(bid, newBid);
			} else {
				throw new RuntimeException("line " + line_no + ": invalid action");
			}

			if (sell_pq.size() == 0 || buy_pq.size() == 0) {
				continue;
			}

			Bid seller = sell_pq.minimum();
			Bid buyer = buy_pq.minimum();

			if (seller.bid <= buyer.bid) {
				sb.append("Transaction: " + buyer.name + " buys a share from " + seller.name + " for " + buyer.bid
						+ " SEK\n");
				sell_pq.deleteMinimum();
				buy_pq.deleteMinimum();
			}
		}

		sb.append("\nOrder book:");

		sb.append("\nSellers: ");
		while (sell_pq.size() > 0) {
			sb.append(sell_pq.minimum().toString() + ", ");
			sell_pq.deleteMinimum();
		}

		sb.append("\nBuyers: ");
		while (buy_pq.size() > 0) {
			sb.append(buy_pq.minimum().toString() + ", ");
			buy_pq.deleteMinimum();
		}

		return sb.toString();
	}

	public static void main(String[] args) throws IOException {
		final BufferedReader actions;
		if (args.length != 1) {
			actions = new BufferedReader(new InputStreamReader(System.in));
		} else {
			actions = new BufferedReader(new FileReader(args[0]));
		}

		List<String> lines = new LinkedList<String>();
		while (true) {
			String line = actions.readLine();
			if (line == null)
				break;
			lines.add(line);
		}
		actions.close();

		System.out.println(pureMain(lines.toArray(new String[lines.size()])));
	}
}
