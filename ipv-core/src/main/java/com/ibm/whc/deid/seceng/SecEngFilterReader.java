/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.seceng;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;

/** A filter reader for Secure Engineering. */
public class SecEngFilterReader extends FilterReader implements SecEngBoundingLimits {
  private int maxCharsToRead = DEFAULT_READ_CHAR_LIMIT;
  private int charsReadThisSession = 0;
  private boolean limitCharsRead = false;

  /**
   * Creates a filter reader.
   *
   * @param in A Reader
   */
  public SecEngFilterReader(Reader in) {
    super(in);
  }

  /**
   * Creates a filter reader and sets the specified max bytes.
   *
   * @param in A Reader
   * @param maxBytes the max bytes
   */
  public SecEngFilterReader(Reader in, int maxBytes) {
    super(in);
    maxCharsToRead = maxBytes;
  }

  @Override
  public int read() throws IOException {
    final int byteRead = super.read();
    if (limitCharsRead && byteRead >= 0) {
      checkMaxCharsReached(1);
    }

    return byteRead;
  }

  @Override
  public int read(char[] cbuf, int off, int len) throws IOException {
    final int charsRead = super.read(cbuf, off, len);
    if (limitCharsRead && charsRead > 0) {
      checkMaxCharsReached(charsRead);
    }
    return charsRead;
  }

  @Override
  public int read(char[] cbuf) throws IOException {
    return read(cbuf, 0, cbuf.length);
  }

  /**
   * Determines if the limit character have been read.
   *
   * @return true or false
   */
  public boolean isLimitCharsRead() {
    return limitCharsRead;
  }

  /**
   * Sets the limit characters to read.
   *
   * @param limitCharsRead the limit
   */
  public void setLimitCharsRead(boolean limitCharsRead) {
    // if changing state, then reset session bytes read.
    // and change state.
    if (this.limitCharsRead != limitCharsRead) {
      this.limitCharsRead = limitCharsRead;
      charsReadThisSession = 0;
    }
  }

  private void checkMaxCharsReached(int bytesRead) throws IOException {
    charsReadThisSession += bytesRead;
    if (charsReadThisSession > maxCharsToRead) {
      throw new IOException("Limit reached");
    }
  }
}
