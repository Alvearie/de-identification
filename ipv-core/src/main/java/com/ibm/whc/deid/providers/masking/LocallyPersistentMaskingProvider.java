/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Locally persistent masking provider.
 *
 */
public class LocallyPersistentMaskingProvider extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = 7135548841123808482L;

  private final MaskingProvider notPersistentMasking;
  private final Map<String, String> persistenceProvider;

  /**
   * Instantiates a new Locally persistent masking provider.
   *
   * @param notPersistentMasking the not persistent masking
   */
  public LocallyPersistentMaskingProvider(MaskingProvider notPersistentMasking) {
    this.notPersistentMasking = notPersistentMasking;
    persistenceProvider = new ConcurrentHashMap<>();
  }

  @Override
  public String mask(String identifier) throws IllegalArgumentException, NullPointerException {
    return mask(identifier, "");
  }

  @Override
  public String mask(final String identifier, final String fieldName)
      throws IllegalArgumentException, NullPointerException {
    if (!persistenceProvider.containsKey(identifier)) {
      String maskedValue = notPersistentMasking.mask(identifier, fieldName);
      synchronized (persistenceProvider) {
        // persistenceProvider.putIfAbsent(identifier, maskedValue);
        if (!persistenceProvider.containsKey(identifier)) {
          persistenceProvider.put(identifier, maskedValue);
        }
      }
    }

    return persistenceProvider.get(identifier);
  }
}
