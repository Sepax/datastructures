package com.lab_2;

public class Bid {
	public final String name;
	public final int bid;

	public Bid(String name, int bid) {
		this.name = name;
		this.bid = bid;
	}

	public int hashCode() {
		return 1 + 23 * bid + 31 * name.hashCode();
	}

	public boolean equals(Object obj) {
		if (obj == null || !(obj instanceof Bid)) {
			return false;
		}

		Bid aBid = (Bid) obj;
		return this.bid == aBid.bid && this.name.equals(aBid.name);
	}

	public String toString() {
		return name + ", " + bid + " SEK";
	}
}
