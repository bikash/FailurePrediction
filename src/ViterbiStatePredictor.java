/*
 * avenir: Predictive analytic based on Hadoop Map Reduce
 * Author: Pranab Ghosh
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License. You may
 * obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0 
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */




import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Mapper.Context;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.chombo.util.Tuple;
import org.chombo.util.Utility;

/**
 * Predicts hidden state sequence, given observation sequence and HMM  model
 * @author pranab
 *
 */
public class ViterbiStatePredictor extends Configured implements Tool {
	@Override
	public int run(String[] args) throws Exception {
		
		
        Job job = new Job(getConf());
        String jobName = "Markov hidden state sequence predictor";
        job.setJobName(jobName);
        Configuration conf = job.getConfiguration();
        conf.set("conf.path","/home/bikash/repos/FailurePrediction/jar/HMM.properties");
        job.setJarByClass(ViterbiStatePredictor.class);

//        FileInputFormat.addInputPath(job, new Path(args[0]));
//        FileOutputFormat.setOutputPath(job, new Path(args[1]));
        //FileInputFormat.addInputPath(job, new Path(" hdfs://localhost:9000/user/bikash/fd/out.txt"));
        //FileOutputFormat.setOutputPath(job, new Path(" hdfs://localhost:9000/user/bikash/fd/out/1"));
        FileInputFormat.setInputPaths(job, new Path("hdfs://localhost:9000/user/bikash/fd/out.txt"));
    	FileOutputFormat.setOutputPath(job, new Path("hdfs://localhost:9000/user/bikash/fd/out/7"));

        Utility.setConfiguration(conf, "HMM");
        job.setMapperClass(ViterbiStatePredictor.StatePredictionMapper.class);

        job.setOutputKeyClass(NullWritable.class);
        job.setOutputValueClass(Text.class);

        job.setNumReduceTasks(0);
        
 

        int status =  job.waitForCompletion(true) ? 0 : 1;
        return status;
	}
	
	/**
	 * @author pranab
	 *
	 */
	public static class StatePredictionMapper extends Mapper<LongWritable, Text, NullWritable, Text> {
		private String fieldDelimRegex;
		private String[] items;
		private int skipFieldCount;
		private int idFieldIndex;
		private HiddenMarkovModel model;
		private ViterbiDecoder decoder;
		private String fieldDelim;
		private Text outVal  = new Text();
		private StringBuilder stBld = new StringBuilder();
		private boolean outputStateOnly;
		private String subFieldDelim;
	    private static final Logger LOG = Logger.getLogger(StatePredictionMapper.class);
		
        /* (non-Javadoc)
         * @see org.apache.hadoop.mapreduce.Mapper#setup(org.apache.hadoop.mapreduce.Mapper.Context)
         */
        protected void setup(Context context) throws IOException, InterruptedException {
        	Configuration conf = context.getConfiguration();
            if (conf.getBoolean("debug.on", false)) {
            	LOG.setLevel(Level.DEBUG);
            }
        	fieldDelimRegex = conf.get("field.delim.regex", ",");
        	fieldDelim = conf.get("field.delim.out", ",");
            skipFieldCount = conf.getInt("skip.field.count", 1);
            idFieldIndex = conf.getInt("id.field.ordinal", 0);
            outputStateOnly = conf.getBoolean("output.state.only", true);
            subFieldDelim = conf.get("sub.field.delim", ":");
            String subFieldDelim1 = conf.get("hmm.model.path", "");
            System.out.println(subFieldDelim1);
        	List<String> lines = Utility.getFileLines(conf, "hmm.model.path");
        	System.out.println(lines);
        	model = new HiddenMarkovModel(lines,  LOG);
        	decoder = new ViterbiDecoder(model, LOG);
        }
        
        /* (non-Javadoc)
         * @see org.apache.hadoop.mapreduce.Mapper#map(KEYIN, VALUEIN, org.apache.hadoop.mapreduce.Mapper.Context)
         */
        protected void map(LongWritable key, Text value, Context context)
        		throws IOException, InterruptedException {
        	items  =  value.toString().split(fieldDelimRegex);
        	System.out.println("ITEMS --> " + value.toString());
        	decoder.initialize(items.length - skipFieldCount);
        	
        	//build state sequence probability matrix and state pointer matrix
        	for (int i = skipFieldCount; i < items.length; ++i) {
        		decoder.nextObservation(items[i]);
        	}
        	
        	//state sequence
        	String[] states = decoder.getStateSequence(); // hidden states [N, L, N, H, H, H, N, L, N, L, N, L, L, N]
        
        	stBld.delete(0, stBld.length());
        	stBld.append(items[idFieldIndex]);
        	if (outputStateOnly) {
        		//states only
	        	for (int i = states.length - 1; i >= 0; --i) {
	        		stBld.append(fieldDelim).append(states[i]); //append , and make a string of hidden states
	        	}
        	} else {
        		//observation followed by state
	        	for (int i = states.length - 1, j = skipFieldCount; i >= 0; --i, ++j) {
	        		stBld.append(fieldDelim).append(items[j]).append(subFieldDelim).append(states[i]);
	        	}
        	}
        	outVal.set(stBld.toString()); //outVal  --> R9L63ZXYH9,L,L,N,N,L,L,L,L,L,H,H
        	
   			context.write(NullWritable.get(),outVal);
        }
        
	}	
	
	public static void main(String[] args) throws Exception {
        int exitCode = ToolRunner.run(new ViterbiStatePredictor(), args);
        System.exit(exitCode);
	}
	

}