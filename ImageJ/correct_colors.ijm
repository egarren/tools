// This macro runs HyperStackReg on all the regex-selected .tif files in a folder
// Added the regex based file selection in v2
//
// Author: Ved P. Sharma,     December 13, 2018

dirS = getDirectory("Choose source Directory");
dirD = getDirectory("Choose destination Directory");

pattern = ".*"; // for selecting all the files in the folder

// different examples of regex-based selection 
//pattern = "01-03.*Pos [3-9].*";
//pattern = ".*Pos [7-9].*";
//pattern = "01-02.*";

filenames = getFileList(dirS);
count = 0;
for (i = 0; i < filenames.length; i++) {
	currFile = dirS+filenames[i];
	if(endsWith(currFile, ".tif") && matches(filenames[i], pattern)) { // process tif files matching regex
		//s = "open=["+dirS+filenames[i]+"] autoscale color_mode=Grayscale rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT";
		//run("Bio-Formats Importer", s);
		open(currFile);
		count++;
// selecting all channels		
		//run("HyperStackReg ", "transformation=Affine show");
// Use the following for selecting specific channels
//		run("HyperStackReg ", "transformation=Affine channel1 channel2 channel3 channel4 show");
		Stack.setDisplayMode("color");
		Stack.setChannel(1);
		run("Blue");
		Stack.setChannel(2);
		run("Green");
		Stack.setChannel(3);
		run("Red");
		Stack.setChannel(4);
		run("Cyan");
		Stack.setDisplayMode("composite");
		saveAs("Tiff", dirD+getTitle());
		//close(); // close registered file
		close(); // close original file
	}
}
print("Number of files processed: "+count);
