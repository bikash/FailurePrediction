/*
 * FPHMM: Failure Prediction Analysis for Hadoop Cluster
 * Author: Bikash Agrawal
 * Organization: University of Stavanger
 * 
*/


import java.io.IOException;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.chombo.util.Utility;

/**
 * Predicts hidden state sequence, given observation sequence and HMM  model
 * @author Bikash
 *
 */
public class ViterbiStatePredictor extends Configured implements Tool {
	@Override
	public int run(String[] args) throws Exception {
		
        Job job = new Job(getConf());
        
        String jobName = "Markov hidden state sequence predictor";
        job.setJobName(jobName);
        Configuration conf = job.getConfiguration();
        //conf.set("fs.default.name", "hdfs://localhost:9000");
        //conf.set("mapred.job.tracker", "localhost:9001");
        conf.set("conf.path","/home/ekstern/haisen/bikash/repos/FailurePrediction/jar/HMM.properties");
        //conf.addResource(new Path("/usr/lib/hadoop/conf/core-site.xml"));
        //conf.addResource(new Path("/usr/lib/hadoop/conf/hdfs-site.xml"));
        //conf.addResource(new Path("/usr/lib/hadoop/conf/mapred-site.xml"));
        //FileSystem fs = FileSystem.get(new URI("hdfs://localhost:8020"),conf);
        
        //conf.set("conf.path","/home/bikash/repos/FailurePrediction/jar/HMM.properties");
        //conf.get("conf.path",new Path(args[2]));
        
        job.setJarByClass(ViterbiStatePredictor.class);

        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));
        //FileInputFormat.setInputPaths(job, new Path("hdfs://haisen20.ux.uis.no:8020/user/haisen/input/out.txt"));
    	//FileOutputFormat.setOutputPath(job, new Path("hdfs://haisen20.ux.uis.no:8020/user/haisen/fd/out/312"));
        //FileInputFormat.setInputPaths(job, new Path("/user/bikash/input/5"));
    	//FileOutputFormat.setOutputPath(job, new Path("/user/bikash/fd/out/5"));
        Utility.setConfiguration(conf, "HMM");
        job.setMapperClass(ViterbiStatePredictor.StatePredictionMapper.class);

        job.setOutputKeyClass(NullWritable.class);
        job.setOutputValueClass(Text.class);

        job.setNumReduceTasks(0);

        int status =  job.waitForCompletion(true) ? 0 : 1;
        return status;
	}
	
	/**
	 * @author Bikash
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
            //String subFieldDelim1 = conf.get("hmm.model.path", "");
            //System.out.println(subFieldDelim1);
        	List<String> lines = Utility.getFileLines(conf, "hmm.model.path");
        	//System.out.println(lines);
        	LOG.debug("lines :" + lines );
        	model = new HiddenMarkovModel(lines,  LOG);
        	decoder = new ViterbiDecoder(model, LOG);
        }
        
        /* (non-Javadoc)
         * @see org.apache.hadoop.mapreduce.Mapper#map(KEYIN, VALUEIN, org.apache.hadoop.mapreduce.Mapper.Context)
         */
        protected void map(LongWritable key, Text value, Context context)
        		throws IOException, InterruptedException {
        	items  =  value.toString().split(fieldDelimRegex);
        	//System.out.println("ITEMS length--> " + items.length);
        	//System.out.println("ITEMS --> " + value.toString());
        	decoder.initialize(items.length - skipFieldCount);
        	//LOG.debug("ITEMS :" + value.toString() );
        	if(items.length > 1)
        	{
	        	int k = 0;
	        	//build state sequence probability matrix and state pointer matrix
	        	for (int i = skipFieldCount; i < items.length; ++i) {
	        		//System.out.println("item --> " + items[i]);
	        		if(items[i] != null && !items[i].isEmpty())
	        			decoder.nextObservation(items[i]);
	        		else
	        			k++;
	        	}
	        	int obs = items.length - skipFieldCount -k;
	        	//System.out.println("obser  --> " + obs);
	        	decoder.setObs(obs);
	        	//state sequence
	        	String[] states = decoder.getStateSequence(); // hidden states [N, L, N, H, H, H, N, L, N, L, N, L, L, N]
	        	//System.out.println("item end --> " + states);
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
	        	//System.out.println("Prediction  --> " + outVal);
	   			context.write(NullWritable.get(),outVal);
        	}
        }
        
	}	
	
	public static void main(String[] args) throws Exception {
        int exitCode = ToolRunner.run(new ViterbiStatePredictor(), args);
        System.exit(exitCode);
	}
	

}