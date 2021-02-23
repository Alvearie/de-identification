/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;

import com.ibm.whc.deid.models.ReversePatternGenerator;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.util.ReversePatternManager;

/**
 * The pseudonym masking provider.
 *
 */
public class PseudonymMaskingProvider extends AbstractMaskingProvider {
  
  private static final long serialVersionUID = -185505347822379112L;

  private final boolean generateViaOptions;
  private final int minLength;
  private final int maxLength;
  private final boolean generateUppercase;
  private final boolean generateLowercase;
  private final boolean generateDigit;
  private final boolean generateSpecial;
  private final boolean generateViaPattern;
  private final String pattern;
  private final String patternLanguageCode;
  private final String patternName;
  private final boolean generateViaHash;
  private final String hashAlgorithm;
  private final HashMaskingProvider hashMaskingProvider;


  /** Instantiates a new pseudonym masking provider. */
  public PseudonymMaskingProvider() {
    this(new PseudonymMaskingProviderConfig());
  }

  public PseudonymMaskingProvider(PseudonymMaskingProviderConfig configuration) {
    this.random = new SecureRandom();
    this.generateViaOptions = configuration.isGenerateViaOptionsEnabled();
    this.minLength = configuration.getGenerateViaOptionsMinLength();
    this.maxLength = configuration.getGenerateViaOptionsMaxLength();
    this.generateUppercase = configuration.isGenerateViaOptionsGenerateUppercase();
    this.generateLowercase = configuration.isGenerateViaOptionsGenerateLowercase();
    this.generateDigit = configuration.isGenerateViaOptionsGenerateDigit();
    this.generateSpecial = configuration.isGenerateViaOptionsGenerateSpecial();
    this.generateViaPattern = configuration.isGenerateViaPatternEnabled();
    this.pattern = configuration.getGenerateViaPatternPattern();
    this.patternLanguageCode = configuration.getGenerateViaPatternLanguageCode();
    this.patternName = configuration.getGenerateViaPatternPatternName();

    this.generateViaHash = configuration.isGenerateViaHashEnabled();
    this.hashAlgorithm = configuration.isGenerateViaHashUseSHA256() ? "SHA-256" : "SHA-512";
    if (this.generateViaHash) {
      HashMaskingProviderConfig hashConfig = new HashMaskingProviderConfig();
      hashConfig.setAlgorithmDefault(this.hashAlgorithm);
      this.hashMaskingProvider = new HashMaskingProvider(hashConfig);
    } else {
      this.hashMaskingProvider = null;
    }
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    // Check if pseudonym should be created via specific options
    if (this.generateViaOptions) {

      String pattern = createPatternFromOptions();
      ReversePatternGenerator patternGenerator =
          ReversePatternManager.getInstance().getPatternByPattern(pattern);

      identifier = patternGenerator.getRandomToken(random);
      return identifier;
    }

    // Check if pseudonym should be created via pattern
    if (this.generateViaPattern) {

      ReversePatternGenerator patternGenerator;
      if (this.pattern != null && !this.pattern.trim().isEmpty()) {
        patternGenerator = ReversePatternManager.getInstance().getPatternByPattern(this.pattern,
            this.patternLanguageCode);
      } else {
        patternGenerator = ReversePatternManager.getInstance()
            .getPatternByResource(this.patternName, this.patternLanguageCode);
      }

      if (patternGenerator == null) {
        return "";
      }

      identifier = patternGenerator.getRandomToken(random);
      return identifier;
    }

    // Check if pseudonym should be created via hash algorithm
    if (this.generateViaHash) {

      identifier = hashMaskingProvider.mask(identifier);
      return identifier;
    }

    return identifier;
  }

  /**
   * Creates a pattern based on the specified options.
   *
   * @return the pattern
   */
  private String createPatternFromOptions() {

    StringBuilder sb = new StringBuilder();
    sb.append("[");
    if (this.generateUppercase) {
      sb.append("\\u");
    }
    if (this.generateLowercase) {
      sb.append("\\l");
    }
    if (this.generateDigit) {
      sb.append("\\d");
    }
    if (this.generateSpecial) {
      sb.append("!");
      sb.append("@");
      sb.append("#");
      sb.append("$");
      sb.append("%");
      sb.append("^");
      sb.append("&");
      sb.append("*");
      sb.append("\\[");
      sb.append("\\]");
      sb.append("\\\\");
      sb.append("/");
      sb.append("?");
      sb.append("\\{");
      sb.append("\\}");
      sb.append("+");
      sb.append("\\-");
      sb.append("_");
    }
    sb.append("]");

    // Append multiplicity
    String multiplicity = "{" + this.minLength
        + (this.minLength != this.maxLength ? (", " + this.maxLength) : "") + "}";
    sb.append(multiplicity);

    return sb.toString();
  }
}
