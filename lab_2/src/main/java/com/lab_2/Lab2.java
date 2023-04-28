package com.lab_2;

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

			if (action.equals("K")) {
				buy_pq.add(new Bid(name, price));
			} else if (action.equals("S")) {
				sell_pq.add(new Bid(name, price));
			} else if (action.equals("NK")) {
				for (int i = 0; i < buy_pq.size(); i++) {
					if (buy_pq.get(i).name.equals(name)) {
						buy_pq.update(buy_pq.get(i), new Bid(name, price));
					}
				}
			} else if (action.equals("NS")) {
				for (int i = 0; i < sell_pq.size(); i++) {
					if (sell_pq.get(i).name.equals(name)) {
						sell_pq.update(sell_pq.get(i), new Bid(name, price));
					}
				}
			} else {
				throw new RuntimeException("line " + line_no + ": invalid action");
			}

			if (sell_pq.size() == 0 || buy_pq.size() == 0)
				continue;

			// TODO:
			// compare the bids of highest priority from each of
			// each priority queues.
			// if the lowest seller price is lower than or equal to
			// the highest buyer price, then remove one bid from
			// each priority queue and add a description of the
			// transaction to the output.

			if (sell_pq.minimum().bid <= buy_pq.minimum().bid) {
				Bid seller = sell_pq.minimum();
				Bid buyer = buy_pq.minimum();
				sb.append("Transaction -> " + "Selling bid: " + seller.toString() + " Buying bid " + buyer.toString()
						+ "\n");
				sell_pq.deleteMinimum();
				buy_pq.deleteMinimum();
			}
		}

		sb.append("Order book:\n");

		sb.append("Sellers: ");
		while (sell_pq.size() > 0) {
			sb.append("Selling bid:" + sell_pq.minimum().toString() + "\n");
			sell_pq.deleteMinimum();
		}

		sb.append("Buyers: ");
		while (buy_pq.size() > 0) {
			sb.append("Buying bid:" + buy_pq.minimum().toString() + "\n");
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
