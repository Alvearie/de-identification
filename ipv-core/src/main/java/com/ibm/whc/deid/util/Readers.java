/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;

public class Readers {
  private static final Readers instance = new Readers();

  private static CSVParser createGenericReader(Reader reader, char separator, char quoteChar)
      throws IOException {
    return CSVFormat.DEFAULT.withDelimiter(separator).withQuote(quoteChar).parse(reader);
  }

  /**
   * Create csv reader from resource csv reader.
   *
   * @param filename the filename
   * @return the csv reader
   */
  public static CSVParser createCSVReaderFromResource(String filename) throws IOException {
    return createCSVReaderFromStream(instance.getClass().getResourceAsStream(filename), ',', '"');
  }

  /**
   * Create csv reader from file csv reader.
   *
   * @param filename the filename
   * @return the csv reader
   * @throws FileNotFoundException the file not found exception
   */
  public static CSVParser createCSVReaderFromFile(String filename) throws IOException {
    if (filename == null || filename.isEmpty())
      return null;

    FileReader reader = new FileReader(filename);
    return createGenericReader(reader, ',', '"');
  }

  /**
   * Create csv reader from stream csv reader.
   *
   * @param stream the stream
   * @return the csv reader
   */
  public static CSVParser createCSVReaderFromStream(InputStream stream) throws IOException {
    return createCSVReaderFromStream(stream, ',', '"');
  }

  /**
   * Create csv reader from stream csv reader.
   *
   * @param stream the stream
   * @param separator the separator
   * @param quoteChar the quote char
   * @return the csv reader
   */
  public static CSVParser createCSVReaderFromStream(InputStream stream, char separator,
      char quoteChar) throws IOException {
    return createGenericReader(new InputStreamReader(stream, "UTF-8"), separator, quoteChar);
  }
}
