/*
 * © Merative US L.P. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config.masking;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Configuration for the Format-Preserving Encryption privacy provider.
 */
@JsonInclude(Include.NON_NULL)
public class FPEMaskingProviderConfig extends MaskingProviderConfig {

  private static final long serialVersionUID = 1L;

  /**
   * Determines the class of input characters to encrypt and how the result should be processed.
   */
  public enum UsageType {

    /**
     * Only characters 0-9 are encrypted. All other characters are considered symbols and retained
     * in place.
     */
    DIGITS(true), 

    /**
     * Only characters a-z are encrypted. All other characters are considered symbols and retained
     * in place.
     */
    LETTERS_LOWER(true), 

    /**
     * Only characters A-Z are encrypted. All other characters are considered symbols and retained
     * in place.
     */
    LETTERS_UPPER(true), 

    /**
     * Only characters a-z and A-Z are encrypted. All other characters are considered symbols and
     * retained in place. Upper and lower case versions of the same letter are considered to be
     * interchangeable. The output letters are lower case.
     */
    LETTERS_INSENSITIVE_AS_LOWER(true),

    /**
     * Only characters a-z and A-Z are encrypted. All other characters are considered symbols and
     * retained in place. Upper and lower case versions of the same letter are considered to be
     * interchangeable. The output letters are upper case.
     */
    LETTERS_INSENSITIVE_AS_UPPER(true), 

    /**
     * Only characters a-z and A-Z are encrypted. All other characters are considered symbols and
     * retained in place. Upper and lower case versions of the same letter are considered to be
     * interchangeable. The output letters have the same case as the original character in that
     * position.
     */
    LETTERS_INSENSITIVE_AS_ORIGINAL(true), 

    /**
     * Only characters a-z and A-Z are encrypted. All other characters are considered symbols and
     * retained in place. Lower case letters and upper case letters are encrypted as separate input
     * sets. Lower case letters are replaced by lower case letters and upper case letters are
     * replaced with upper case letters.
     */
    LETTERS_SENSITIVE(false),

    /**
     * Only characters 0-9 and a-z are encrypted. All other characters are considered symbols and
     * retained in place. Digits and letters are considered to be from the same character set. A
     * digit in the original value could be replaced with a lower case letter in the output and
     * vice-versa.
     */
    DIGITS_LETTERS_LOWER(true),

    /**
     * Only characters 0-9 and A-Z are encrypted. All other characters are considered symbols and
     * retained in place. Digits and letters are considered to be from the same character set. A
     * digit in the original value could be replaced with an upper case letter in the output and
     * vice-versa.
     */
    DIGITS_LETTERS_UPPER(true),

    /**
     * Only characters 0-9, a-z, and A-Z are encrypted. All other characters are considered symbols
     * and retained in place. Upper and lower case versions of the same letter are considered to be
     * interchangeable. Digits and letters are considered to be from the same character set. A digit
     * in the original value could be replaced with a letter in the output and vice-versa. Letters
     * in the output are in lower case.
     */
    DIGITS_LETTERS_INSENSITIVE_AS_LOWER(true),

    /**
     * Only characters 0-9, a-z, and A-Z are encrypted. All other characters are considered symbols
     * and retained in place. Upper and lower case versions of the same letter are considered to be
     * interchangeable. Digits and letters are considered to be from the same character set. A digit
     * in the original value could be replaced with a letter in the output and vice-versa. Letters
     * in the output are in upper case.
     */
    DIGITS_LETTERS_INSENSITIVE_AS_UPPER(true),

    /**
     * Only characters 0-9, a-z, and A-Z are encrypted. All other characters are considered symbols
     * and retained in place. Digits, lower case letters, and upper case letters are all encrypted
     * as separate input sets. Digits are replaced with digits, lower case letters are replaced with
     * lower case letters, and upper case letters are replaced with upper case letters.
     */
    DIGITS_LETTERS_SENSITIVE(false)
    ;

    private final boolean padding;

    private UsageType(boolean p) {
      padding = p;
    }
    
    public boolean supportsPadding() {
      return padding;
    }
  }

  /**
   * Determines how data underflow is handled.
   */
  public enum Pad {

    /**
     * No padding - error if value has less than the minimum characters for the associated algorithm
     */
    NONE,

    /**
     * Add pad characters as necessary to the beginning of the value. Note that padding characters
     * are encrypted and retained in the output value and any symbols in the input are shifted
     * accordingly in the output. Using pad characters means the encryption is not compliant with
     * NIST FPE FF3-1.
     */
    FRONT,

    /**
     * add pad characters as necessary to the end of the value. Note that padding characters are
     * encrypted and retained in the output value and any symbols in the input are shifted
     * accordingly in the output. Using pad characters means the encryption is not compliant with
     * NIST FPE FF3-1.
     */
    BACK
  }

  public static final UsageType DEFAULT_USAGE_TYPE = UsageType.DIGITS;
  public static final Pad DEFAULT_PADDING = Pad.NONE;
  public static final Pattern ENC_KEY_CONTENT_PATTERN = Pattern.compile("[0-9a-f]+");

  private String key;
  private String tweak;
  private UsageType inputType;
  private Pad padding;

  public FPEMaskingProviderConfig() {
    inputType = DEFAULT_USAGE_TYPE;
    padding = DEFAULT_PADDING;
    type = MaskingProviderType.FPE;
  }

  /**
   * 
   * @return the main encryption key
   */
  public String getKey() {
    return key;
  }

  public void setKey(String key) {
    this.key = key;
  }

  /**
   * 
   * @return the secondary encryption key
   */
  public String getTweak() {
    return tweak;
  }

  public void setTweak(String tweak) {
    this.tweak = tweak;
  }

  /**
   * 
   * @return the strategy for parsing the input and performing the encryption
   */
  public UsageType getInputType() {
    return inputType;
  }

  public void setInputType(UsageType inputType) {
    this.inputType = inputType == null ? DEFAULT_USAGE_TYPE : inputType;
  }

  /**
   * 
   * @return how insufficient input data situations are to be handled
   */
  public Pad getPadding() {
    return padding;
  }

  public void setPadding(Pad padding) {
    this.padding = padding == null ? DEFAULT_PADDING : padding;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + Objects.hash(inputType, key, padding, tweak);
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (!(obj instanceof FPEMaskingProviderConfig)) {
      return false;
    }
    FPEMaskingProviderConfig other = (FPEMaskingProviderConfig) obj;
    return inputType == other.inputType && Objects.equals(key, other.key)
        && padding == other.padding && Objects.equals(tweak, other.tweak);
  }

  @Override
  public void validate(DeidMaskingConfig maskingConfig)
      throws InvalidMaskingConfigurationException {

    super.validate(maskingConfig);

    int length = key == null ? 0 : key.length();
    if (length != 32 && length != 48 && length != 64) {
      throw new InvalidMaskingConfigurationException("`key` must be 32, 48, or 64 characters");
    }
    Matcher matcher = ENC_KEY_CONTENT_PATTERN.matcher(key);
    if (!matcher.matches()) {
      throw new InvalidMaskingConfigurationException(
          "`key` must contain only characters 0-9 and a-f");
    }

    length = tweak == null ? 0 : tweak.length();
    if (length != 16) {
      throw new InvalidMaskingConfigurationException("`tweak` must be 16 characters");
    }
    matcher = ENC_KEY_CONTENT_PATTERN.matcher(tweak);
    if (!matcher.matches()) {
      throw new InvalidMaskingConfigurationException(
          "`tweak` must contain only characters 0-9 and a-f");
    }

    if (padding != Pad.NONE && !inputType.supportsPadding()) {
      throw new InvalidMaskingConfigurationException("`padding` has the value " + padding.name()
          + " but the `inputType` value " + inputType.name() + " does not support padding");
    }
  }
}
