/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.EmailMaskingProviderConfig;
import com.ibm.whc.deid.util.RandomGenerators;

/**
 * The type Email masking provider.
 *
 */
public class EmailMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -2162447900677344508L;

  private final int preserveDomains;
  private final int unspecifiedValueHandling;
  private final String unspecifiedValueReturnMessage;

  private int nameCharacters;


  /**
   * Instantiates a new Email masking provider.
   *
   * @param configuration the configuration
   */
  public EmailMaskingProvider() {
    this(new EmailMaskingProviderConfig());
  }


  /**
   * Instantiates a new Email masking provider.
   *
   * @param random the random
   * @param configuration the configuration
   */
  public EmailMaskingProvider(EmailMaskingProviderConfig configuration) {
    this.preserveDomains = configuration.getPreserveDomains();
    this.nameCharacters = configuration.getNameLength();

    this.unspecifiedValueHandling = configuration.getUnspecifiedValueHandling();
    this.unspecifiedValueReturnMessage = configuration.getUnspecifiedValueReturnMessage();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    String domain;

    int index = identifier.indexOf('@');
    if (index == -1) {
      debugFaultyInput("index");
      if (unspecifiedValueHandling == 2) {
        domain = RandomGenerators.randomUIDGenerator(8) + "." + RandomGenerators.getRandomTLD();
      } else if (unspecifiedValueHandling == 3) {
        return unspecifiedValueReturnMessage;
      } else {
        return null;
      }
    } else {
      domain = identifier.substring(identifier.indexOf('@') + 1);
    }

    StringBuilder builder =
        new StringBuilder(RandomGenerators.randomUsernameAndDomain(nameCharacters));
    builder.append("@");
    builder.append(RandomGenerators.randomHostnameGenerator(domain, this.preserveDomains));
    return builder.toString();
  }
}
