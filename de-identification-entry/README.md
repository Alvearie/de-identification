# De-identification code-level entry point

This project provides an entry point to interact with de-identification functions provided by the De-Identification service at the Java level without an external process.

## Usage 

Typical usage pattern is to perform this call once to obtain a processor object that will de-identify documents based on the given configuration.

````
DeidProcessor proc = DeidEntry.getDeidProcessor(maskingConfiguation);
````

The maskingConfiguation is a string containing the JSON serialized version of the associated DeId Java objects that describe the masking operations to be performed.
  
Once a DeidProcessor instance is available, the following method can be called multiple times to de-identify input JSON documents.

````
List<String> out = proc.process(List<String>);
````

## Integration

The De-Identification service build process produces a **de-identification-entry-_version_-jar-with-dependencies.jar** file.  Add this jar at the end of the classpath for your parent Java process.  This jar file contains all the classes required to use the De-Identification service as a Java utility jar along with all the classes for its required dependencies.  
