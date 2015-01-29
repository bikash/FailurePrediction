#!/usr/bin/ruby

require './util.rb'      

custCount = ARGV[0].to_i
events = ['1','2','3','4','5','6','7']


timestampStart = 315586167
open('myfile.txt', 'a') { |f|
1.upto custCount do
	timestampStart = timestampStart + 1
	timestamp = timestampStart
	num_events = 1 + rand(3)
	cust_events = []
	1.upto num_events do
		indx = rand(events.size)
		ev = events[indx]
		cust_events << ev
		if (rand(10) < 3)
			r = 1 + rand(3)
			1.upto r do
				indx = (indx / 3) * 3 + rand(2)
				ev = events[indx]
				cust_events << ev
			end
		end
	end
	
	f.puts "#{timestamp},#{cust_events.join(',')}"
end
}