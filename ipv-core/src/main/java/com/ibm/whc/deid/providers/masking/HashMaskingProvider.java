/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import org.apache.commons.lang3.StringUtils;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;

/** The type Hash masking provider. */
public class HashMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 7924730647129367168L;

  private final char[] hexArray = "0123456789ABCDEF".toCharArray();

  private final String algorithm;
  private final boolean hashingOffsetOffsetMask;
  private final boolean hashingOffsetOffsetMaskDelete;
  private final int hashingOffsetBegin;
  private final int hashingOffsetEnd;
  private final int hashingOffsetInvalidOffsetValue;
  private final String hashSaltValue;

  /** Instantiates a new Hash masking provider. */
  public HashMaskingProvider() {
    this(new HashMaskingProviderConfig());
  }

  /**
   * Instantiates a new Hash masking provider.
   *
   * @param configuration the configuration
   */
  public HashMaskingProvider(HashMaskingProviderConfig config) {
    super(config);
    this.algorithm = config.getAlgorithmDefault();
    this.hashingOffsetOffsetMask = config.isOffsetOffsetMask();
    this.hashingOffsetOffsetMaskDelete = config.isOffsetOffsetMaskDelete();
    this.hashingOffsetBegin = config.getOffsetBegin();
    this.hashingOffsetEnd = config.getOffsetEnd();
    this.hashingOffsetInvalidOffsetValue = config.getOffsetInvalidOffsetValue();
    this.hashSaltValue = config.getSalt();
  }

  private String bytesToHex(byte[] bytes) {
    char[] hexChars = new char[bytes.length * 2];
    for (int j = 0; j < bytes.length; j++) {
      int v = bytes[j] & 0xFF;
      hexChars[j * 2] = hexArray[v >>> 4];
      hexChars[j * 2 + 1] = hexArray[v & 0x0F];
    }

    return new String(hexChars);
  }

  private String getHashValue(MessageDigest md, String identifier) {
    md.update(identifier.getBytes());
    byte[] shaDig = md.digest();
    return bytesToHex(shaDig);
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    MessageDigest md;
    try {
      md = MessageDigest.getInstance(this.algorithm);
    } catch (NoSuchAlgorithmException e) {
      logException(e);
      throw new RuntimeException(e);
    }

    if (hashingOffsetOffsetMask) {
      // Get the begin and end offsets. The values default to-1 if not
      // specified, in this case use the first or last character as
      // defaults.
      int beginOffset = hashingOffsetBegin != -1 ? hashingOffsetBegin : 0;
      int endOffset = hashingOffsetEnd != -1 ? hashingOffsetEnd : identifier.length();

      // Ensure we have valid offsets, if not return the value
      // specified by the "hashing.offset.invalidOffsetValue" option
      // Valid options are: 1 - return null; 2 - return empty string;
      // 3 - return the hash value of the entire data element
      if (beginOffset < 0 || endOffset > identifier.length() || endOffset <= beginOffset) {
        if (hashingOffsetInvalidOffsetValue == 2)
          return "";
        else if (hashingOffsetInvalidOffsetValue == 3)
          // Apply salt and hash
          return getHashValue(md, identifier + this.hashSaltValue);
        else
          return null;
      }

      // Hash the portion of the input value specified by the offsets
      // and then replace that portion in the original value with the
      // hashed value.
      String headNonHashedValue = StringUtils.substring(identifier, 0, beginOffset);
      String tailNonHashedValue = StringUtils.substring(identifier, endOffset, identifier.length());
      String partToHash = StringUtils.substring(identifier, beginOffset, endOffset);

      // Apply salt and hash
      if (hashingOffsetOffsetMaskDelete) {
        headNonHashedValue = "";
        tailNonHashedValue = "";
      }
      return headNonHashedValue + getHashValue(md, partToHash + this.hashSaltValue)
          + tailNonHashedValue;
    }

    // Apply salt and hash
    return getHashValue(md, identifier + this.hashSaltValue);
  }
}
