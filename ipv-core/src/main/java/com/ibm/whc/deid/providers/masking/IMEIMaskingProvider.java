/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.ibm.whc.deid.providers.identifiers.IMEIIdentifier;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import com.ibm.whc.deid.util.IMEIManager;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.RandomGenerators;

public class IMEIMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = -5189348513611132622L;

  private final IMEIIdentifier imeiIdentifier;
  private final boolean preserveTAC;

  protected transient volatile IMEIManager imeiResourceManager = null;

  /**
   * Instantiates a new IMEI masking provider.
   *
   * @param configuration the configuration of this privacy provider
   * @param tenantId the identifier of the tenant associated with this request
   * @param localizationProperty location of the localization property file
   */
  public IMEIMaskingProvider(IMEIMaskingProviderConfig config, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, config);
    this.imeiIdentifier = new IMEIIdentifier(tenantId, localizationProperty);
    this.preserveTAC = config.getPreserveTAC();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    if (preserveTAC && !imeiIdentifier.isOfThisType(identifier)) {
      return applyUnexpectedValueHandling(identifier,
          () -> generateValue(getIMEIManager().getRandomKey()));
    }

    return generateValue(
        this.preserveTAC ? identifier.substring(0, 8) : getIMEIManager().getRandomKey());
  }

  private String generateValue(String tac) {
    String body = tac + RandomGenerators.generateRandomDigitSequence(6);
    body += (char) ('0' + RandomGenerators.luhnCheckDigit(body));
    return body;
  }

  protected IMEIManager getIMEIManager() {
    if (imeiResourceManager == null) {
      imeiResourceManager = (IMEIManager) ManagerFactory.getInstance().getManager(tenantId,
          Resource.TACDB, null, localizationProperty);
    }
    return imeiResourceManager;
  }
}
