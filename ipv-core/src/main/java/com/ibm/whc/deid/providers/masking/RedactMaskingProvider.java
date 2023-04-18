/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;

public class RedactMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 5183388096545702258L;

  private final boolean preserveLength;
  private final String replacementCharacter;

  /** Instantiates a new Redact masking provider. */
  public RedactMaskingProvider() {
    this(new RedactMaskingProviderConfig());
  }

  /**
   * Instantiates a new Redact masking provider.
   *
   * @param configuration the configuration
   */
  public RedactMaskingProvider(RedactMaskingProviderConfig configuration) {
    super(configuration);
    this.preserveLength = configuration.isPreserveLength();
    this.replacementCharacter = configuration.getReplaceCharacter();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (preserveLength) {
      StringBuilder builder = new StringBuilder(identifier.length());
      for (int i = 0; i < identifier.length(); ++i) {
        builder.append(replacementCharacter);
      }
      return builder.toString();
    } else {
      return replacementCharacter;
    }
  }
}
