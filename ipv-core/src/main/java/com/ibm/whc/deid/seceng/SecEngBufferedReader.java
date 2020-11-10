/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.seceng;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

/** A buffered reader for Secure Engineering. */
public class SecEngBufferedReader extends BufferedReader {
  private final SecEngFilterReader secEngReader;

  /**
   * Creates a buffering character-input stream that uses an input buffer of the specified size.
   *
   * @param in A Reader
   * @param sz Input-buffer size
   */
  public SecEngBufferedReader(Reader in, int sz) {
    this(new SecEngFilterReader(in), sz);
  }

  /**
   * Creates a buffering character-input stream that uses a default-sized input buffer.
   *
   * @param in A Reader
   */
  public SecEngBufferedReader(Reader in) {
    this(new SecEngFilterReader(in));
  }

  /**
   * Creates a buffering character-input stream that uses a default-sized input buffer.
   *
   * @param in A Reader
   */
  public SecEngBufferedReader(SecEngFilterReader in) {
    super(in);
    secEngReader = in;
  }

  /**
   * Creates a buffering character-input stream that uses an input buffer of the specified size.
   *
   * @param in A Reader
   * @param sz Input-buffer size
   */
  public SecEngBufferedReader(SecEngFilterReader in, int sz) {
    super(in, sz);
    secEngReader = in;
  }

  @Override
  public String readLine() throws IOException {
    // "Mark" the BoundedReader to start tracking bytes read.
    // To ensure we don't end up with a runaway readLine we limit the max
    // bytes that a readline can read
    try {
      secEngReader.setLimitCharsRead(true);
      return super.readLine();
    } finally {
      // All done, turn off limiting of bytes reads until next readLine
      secEngReader.setLimitCharsRead(false);
    }
  }
}
