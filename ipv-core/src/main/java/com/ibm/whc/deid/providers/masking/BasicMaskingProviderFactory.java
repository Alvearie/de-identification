/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.providers.masking.fhir.DateDependencyMaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.FHIRMaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.GenericMaskingProvider;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ATCMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.AddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.BinningMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ConditionalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ContinentMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CountryMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CountyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.CreditCardMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateDependencyMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.EmailMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.GUIDMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.GenderMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.GeneralizeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HashMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.HospitalMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.IBANMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv10MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ICDv9MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.IMEIMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.IPAddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.LatitudeLongitudeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MACAddressMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaritalStatusMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.MaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.NullMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.NumberVarianceMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.PhoneMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.PseudonymMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RaceEthnicityMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.RedactMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ReligionMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ReplaceMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUKMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.SSNUSMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.SWIFTMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.StatesUSMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.URLMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.VINMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.ZIPCodeMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * The type Masking provider factory.
 *
 */
public class BasicMaskingProviderFactory implements Serializable, MaskingProviderFactory {

  private static final long serialVersionUID = -7454645556196383954L;

  /**
   * Per-tenant map of masking provider configurations to masking provider objects
   */
  private final Map<String,ConcurrentHashMap<MaskingProviderConfig,MaskingProvider>> maskingProvidersCache;

  public BasicMaskingProviderFactory() {
    this(null, null);
  }

  /**
   * Instantiates a new masking provider factory.
   *
   * @param deidMaskingConfig
   * @param configurationManager the configuration manager
   * @param identifiedTypes the identified types
   */
  public BasicMaskingProviderFactory(DeidMaskingConfig deidMaskingConfig,
      Map<String, ProviderType> identifiedTypes) {
    // note - NULL is a possible key for this map
    maskingProvidersCache = new HashMap<>();
  }

  /**
   * Remove masking provider cache for a tenant.
   *
   * @param tenantId
   */
  public synchronized void invalidateCache(String tenantId) {
    maskingProvidersCache.remove(tenantId);
  }

  private synchronized ConcurrentHashMap<MaskingProviderConfig, MaskingProvider> getPerTenantCache(
      String tenantId) {
    ConcurrentHashMap<MaskingProviderConfig, MaskingProvider> maskingProviders =
        maskingProvidersCache.get(tenantId);
    if (maskingProviders == null) {
      maskingProviders = new ConcurrentHashMap<>();
      maskingProvidersCache.put(tenantId, maskingProviders);
    }
    return maskingProviders;
  }

  @Override
  public MaskingProvider getProviderFromType(MaskingProviderTypes providerType,
      DeidMaskingConfig deidMaskingConfig, MaskingProviderConfig config, String tenantId, String localizationProperty) {

    // Note - as per documentation of the MaskingProviderFactory interface, allow
    // NullPointerException to be thrown if providerType or config are null
    
    // Check the cache first

    ConcurrentHashMap<MaskingProviderConfig, MaskingProvider> maskingProviderCache = getPerTenantCache(tenantId);
    MaskingProvider provider = maskingProviderCache.get(config);
    if (provider != null) {
      return provider;
    }
		provider = getNewProviderFromType(providerType, deidMaskingConfig, config, tenantId, localizationProperty);
    maskingProviderCache.put(config, provider);
    return provider;
  }

  protected MaskingProvider getNewProviderFromType(MaskingProviderTypes providerType,
      DeidMaskingConfig deidMaskingConfig, MaskingProviderConfig config, String tenantId, String localizationProperty) {
    MaskingProvider provider = null;
    switch ((MaskingProviderType) providerType) {
      case ADDRESS:
        provider =
            new AddressMaskingProvider((AddressMaskingProviderConfig) config, tenantId, this);
        break;
      case ATC:
        provider = new ATCMaskingProvider((ATCMaskingProviderConfig) config, tenantId);
        break;
      case BINNING:
        provider = new BinningMaskingProvider((BinningMaskingProviderConfig) config);
        break;
      case CITY:
        provider = new CityMaskingProvider((CityMaskingProviderConfig) config, tenantId, localizationProperty);
        break;
      case CONDITIONAL:
        provider = new ConditionalMaskingProvider((ConditionalMaskingProviderConfig) config,
            tenantId, deidMaskingConfig);
        break;
      case CONTINENT:
        provider = new ContinentMaskingProvider((ContinentMaskingProviderConfig) config, tenantId, localizationProperty);
        break;
      case COUNTY:
        provider = new CountyMaskingProvider((CountyMaskingProviderConfig) config, tenantId);
        break;
      case COUNTRY:
        provider = new CountryMaskingProvider((CountryMaskingProviderConfig) config, tenantId);
        break;
      case CREDIT_CARD:
        provider = new CreditCardMaskingProvider((CreditCardMaskingProviderConfig) config);
        break;
      case DATEDEPENDENCY:
        provider = new DateDependencyMaskingProvider((DateDependencyMaskingProviderConfig) config,
            deidMaskingConfig);
        break;
      case DATETIME:
        provider = new DateTimeMaskingProvider((DateTimeMaskingProviderConfig) config);
        break;
      case EMAIL:
        provider = new EmailMaskingProvider((EmailMaskingProviderConfig) config);
        break;
      case FHIR:
        provider = new FHIRMaskingProvider(deidMaskingConfig, this, tenantId);
        break;
      case GEN:
        provider = new GenericMaskingProvider(deidMaskingConfig, this, tenantId);
        break;
      case GENDER:
        provider = new GenderMaskingProvider((GenderMaskingProviderConfig) config, tenantId);
        break;
      case GENERALIZE:
        provider = new GeneralizeMaskingProvider((GeneralizeMaskingProviderConfig) config);
        break;
      case GUID:
        provider = new GUIDMaskingProvider((GUIDMaskingProviderConfig) config);
        break;
      case HASH:
        provider = new HashMaskingProvider((HashMaskingProviderConfig) config);
        break;
      case HOSPITAL:
        provider = new HospitalMaskingProvider((HospitalMaskingProviderConfig) config, tenantId);
        break;
      case IBAN:
        provider = new IBANMaskingProvider((IBANMaskingProviderConfig) config);
        break;
      case ICDV9:
        provider = new ICDv9MaskingProvider((ICDv9MaskingProviderConfig) config, tenantId);
        break;
      case ICDV10:
        provider = new ICDv10MaskingProvider((ICDv10MaskingProviderConfig) config, tenantId);
        break;
      case IMEI:
        provider = new IMEIMaskingProvider((IMEIMaskingProviderConfig) config);
        break;
      case IP_ADDRESS:
        provider = new IPAddressMaskingProvider((IPAddressMaskingProviderConfig) config);
        break;
      case LATITUDE_LONGITUDE:
        provider =
            new LatitudeLongitudeMaskingProvider((LatitudeLongitudeMaskingProviderConfig) config);
        break;
      case MAC_ADDRESS:
        provider = new MACAddressMaskingProvider((MACAddressMaskingProviderConfig) config);
        break;
      case MAINTAIN:
        provider = new MaintainMaskingProvider();
        break;
      case MARITAL:
        provider =
            new MaritalStatusMaskingProvider((MaritalStatusMaskingProviderConfig) config, tenantId);
        break;
      case NAME:
        provider = new NameMaskingProvider((NameMaskingProviderConfig) config, tenantId, localizationProperty);
        break;
      case NULL:
        provider = new NullMaskingProvider((NullMaskingProviderConfig) config);
        break;
      case NUMBERVARIANCE:
        provider = new NumberVarianceMaskingProvider((NumberVarianceMaskingProviderConfig) config);
        break;
      case OCCUPATION:
        provider =
            new OccupationMaskingProvider((OccupationMaskingProviderConfig) config, tenantId);
        break;
      case PHONE:
        provider = new PhoneMaskingProvider((PhoneMaskingProviderConfig) config, tenantId);
        break;
      case PSEUDONYM:
        provider = new PseudonymMaskingProvider((PseudonymMaskingProviderConfig) config);
        break;
      case RACE:
        provider =
            new RaceEthnicityMaskingProvider((RaceEthnicityMaskingProviderConfig) config, tenantId);
        break;
      case RANDOM:
        provider = new RandomMaskingProvider();
        break;
      case REDACT:
        provider = new RedactMaskingProvider((RedactMaskingProviderConfig) config);
        break;
      case RELIGION:
        provider = new ReligionMaskingProvider((ReligionMaskingProviderConfig) config, tenantId, localizationProperty);
        break;
      case REPLACE:
        provider = new ReplaceMaskingProvider((ReplaceMaskingProviderConfig) config);
        break;
      case SSN_UK:
        provider = new SSNUKMaskingProvider((SSNUKMaskingProviderConfig) config);
        break;
      case SSN_US:
        provider = new SSNUSMaskingProvider((SSNUSMaskingProviderConfig) config);
        break;
      case STATE_US:
        provider = new StatesUSMaskingProvider((StatesUSMaskingProviderConfig) config, tenantId);
        break;
      case SWIFT:
        provider = new SWIFTCodeMaskingProvider((SWIFTMaskingProviderConfig) config, tenantId);
        break;
      case URL:
        provider =
            new URLMaskingProvider((URLMaskingProviderConfig) config, tenantId, deidMaskingConfig);
        break;
      case VIN:
        provider = new VINMaskingProvider((VINMaskingProviderConfig) config, tenantId);
        break;
      case ZIPCODE:
        provider = new ZIPCodeMaskingProvider((ZIPCodeMaskingProviderConfig) config, tenantId);
        break;
      default:
        throw new IllegalArgumentException("Unsupported provider type" + providerType);
    }
    return provider;
  }
}
