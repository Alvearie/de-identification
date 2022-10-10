/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.metadata;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionGroupModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionModel.OptionType;
import com.ibm.whc.deid.providers.masking.metadata.model.ConfigurationOptionPossibleValueModel;
import com.ibm.whc.deid.providers.masking.metadata.model.MaskingProviderMetadataModel;
import com.ibm.whc.deid.shared.pojo.config.masking.UnexpectedMaskingInputHandler;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderTypes;

/**
 * Base class for classes implementing the MaskingProviderMetadataBuilder interface. Provides common
 * support for functions all such classes must provide and utilities for providing other functions,
 * including interaction with the common ResourceBundle and resource conventions used for
 * translatable information.
 * 
 * <p>
 * This base class is thread-safe.
 */
public class MaskingProviderMetadataBuilderBase implements MaskingProviderMetadataBuilder {

  protected static final String DEFAULT_DOCUMENT_LINK =
      "https://github.com/Alvearie/de-identification/blob/master/docs/masking-providers.md";

  private static final String METADATA_RESOURCE_BUNDLE =
      "com.ibm.whc.deid.providers.masking.metadata.MaskingProviderMetadataBundle";

  /**
   * Obtains information about a masking provider type. This implementation provides a template for
   * building the information. Subclasses can override any parts of the template for which the
   * provider they support has different requirements.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the translatable or human-readable parts of the
   *        information are to be returned
   * 
   * @return the available information in structured form
   */
  @Override
  public MaskingProviderMetadataModel getMaskingProviderMetadata(MaskingProviderTypes provider,
      Locale locale) {
    MaskingProviderMetadataModel md = new MaskingProviderMetadataModel();
    md.setName(getName(provider));
    md.setDescription(getDescription(provider, locale));
    md.setCategory(provider.getCategory());
    md.setDocLink(getDocumentLink(provider, locale));
    List<ConfigurationOptionModel> commonOptions = null;
    if (supportsCommonOptions(provider)) {
      commonOptions = getCommonOptions(getMetadataResourceBundle(locale));
    }
    List<ConfigurationOptionModel> options = getOptions(provider, locale);
    if (options == null) {
      md.setOptions(commonOptions);
    } else { 
      if (commonOptions != null) {
        options.addAll(commonOptions);
      }
      md.setOptions(options);
    }
    md.setOptionGroups(getOptionGroups(provider, locale));
    return md;
  }

  /**
   * Returns the name of the provider as required in masking provider metadata.
   * 
   * @param provider the provider about which information is required
   * 
   * @return the name to include in the provider metadata
   */
  protected String getName(MaskingProviderTypes provider) {
    return provider.getIdentifier();
  }

  /**
   * Obtains the common resource bundle used to manage translated masking provider metadata
   * information
   * 
   * @param locale the preferred language for the information - the ResourceBundle substitutes the
   *        best available substitute if an item is not available in the given language
   * 
   * @return the interface to the ResourceBundle
   */
  protected ResourceBundle getMetadataResourceBundle(Locale locale) {
    return ResourceBundle.getBundle(METADATA_RESOURCE_BUNDLE, locale);
  }

  /**
   * Returns a user-friendly description of the masking provider - typically a 1-3 sentences.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the description should be returned. If
   *        information is not available in this language, the best available substitute is used.
   * 
   * @return the user-friendly description, translated as possible
   */
  protected String getDescription(MaskingProviderTypes provider, Locale locale) {
    return getMetadataResourceBundle(locale).getString(provider.getIdentifier() + "_desc");
  }

  /**
   * Obtains a URL for detailed documentation about a masking provider. This implementation returns
   * the common masking provider information page with an anchor for to the section for the specific
   * provider.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the description should be returned. This
   *        implementation does not use the given locale, but a subclass might.
   * 
   * @return the URL to the information
   */
  protected String getDocumentLink(MaskingProviderTypes provider, Locale locale) {
    // lowercase provider identifiers are used as anchors in the documentation
    String id = provider.getIdentifier().toLowerCase();
    String link = DEFAULT_DOCUMENT_LINK;
    StringBuilder buffer = new StringBuilder(link.length() + 1 + id.length());
    buffer.append(link).append('#').append(id);
    return buffer.toString();
  }

  /**
   * Returns information about the configuration options supported by a masking provider. This
   * implementation returns <i>null</i>. If a masking provider supports configuration options, a
   * subclass that handles that provider type should override this method.
   * 
   * <p>
   * See also {@link #getOptionGroups(MaskingProviderTypes, Locale)}. Subclasses might return
   * additional configuration options organized into related groups for user convenience.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the description should be returned. If
   *        information is not available in this language, the best available substitute is used.
   * 
   * @return Structured data to describe configuration options supported by the given masking
   *         provider or <i>null</i> if the provider does not support any configuration options or
   *         no options beyond the defaults supported by all providers.
   */
  protected List<ConfigurationOptionModel> getOptions(MaskingProviderTypes provider, Locale locale) {
    return null;
  }

  /**
   * Returns information about groups of related configuration options supported by a masking
   * provider. This implementation returns <i>null</i>. If a masking provider supports groups of
   * configuration options, a subclass that handles that provider type should override this method.
   * 
   * <p>
   * See also {@link #getOption(MaskingProviderTypes, Locale)}. Subclasses might return additional
   * configuration options that are not organized into any particular groups.
   * 
   * @param provider the provider about which information is required
   * 
   * @param locale the preferred language in which the description should be returned. If
   *        information is not available in this language, the best available substitute is used.
   * 
   * @return Structured data to describe a group of related configuration options and the individual
   *         options that are part of that group supported by the given masking provider or
   *         <i>null</i> if the provider does not support any groups of configuration options.
   */
  protected List<ConfigurationOptionGroupModel> getOptionGroups(MaskingProviderTypes provider,
      Locale locale) {
    return null;
  }

  /**
   * A convenience method to build the structured data for a configuration option using the
   * information in the common ResourceBundle.
   * 
   * @param identifier the identifier or name of the masking provider for which information is
   *        required
   * 
   * @param optName the internal name of the configuration option
   * 
   * @param bundle the resource bundle from which information is to be obtained - this is passed in
   *        to avoid re-obtaining this from the ResourceBundle cache multiple times if this method
   *        is called multiple times for multiple options
   * 
   * @param type the data type of the configuration option
   * 
   * @param dflt the default value for the configuration option or <i>null</i> if not default is
   *        supported or required
   * 
   * @return the structured data object describing the configuration option
   */
  protected ConfigurationOptionModel buildOption(String identifier, String optName,
      ResourceBundle bundle, OptionType type, String dflt) {
    return new ConfigurationOptionModel(optName,
        bundle.getString(buildDisplayProperty(identifier, optName)), type,
        bundle.getString(buildDescriptionProperty(identifier, optName)), dflt);
  }

  /**
   * Obtains the name of the resource in the common resource bundle that would provide the
   * user-friendly name of a configuration option.
   * 
   * @param identifier the name of the masking provider to which the configuration option pertains
   * 
   * @param optName the internal name of the configuration option
   * 
   * @return the resource name to use to obtain the translated user-friendly name from the resource
   *         bundle
   */
  protected String buildDisplayProperty(String identifier, String optName) {
    return buildProperty(identifier, "_opt_display_", optName);
  }

  /**
   * Obtains the name of the resource in the common resource bundle that would provide the
   * user-friendly description of a configuration option.
   * 
   * @param identifier the name of the masking provider to which the configuration option pertains
   * 
   * @param optName the internal name of the configuration option
   * 
   * @return the resource name to use to obtain the translated user-friendly description from the
   *         resource bundle
   */
  protected String buildDescriptionProperty(String identifier, String optName) {
    return buildProperty(identifier, "_opt_desc_", optName);
  }

  /**
   * Builds a resource name following the conventions in the common resource bundle for a particular
   * data item that describes a configuration option.
   * 
   * @param identifier the name of the masking provider to which the configuration option pertains
   * 
   * @param propertyType a string that identifies the type of information required about the
   *        configuration option
   * 
   * @param optName the internal name of the configuration option
   * 
   * @return the resource name to use to obtain the translated user-friendly information from the
   *         resource bundle
   */
  private String buildProperty(String identifier, String propertyType, String optName) {
    StringBuilder buffer =
        new StringBuilder(identifier.length() + propertyType.length() + optName.length());
    buffer.append(identifier).append(propertyType).append(optName);
    return buffer.toString();
  }

  /**
   * Obtains information about a supported value for a masking provider configuration option.
   * 
   * @param identifier the name of the masking provider about which information is required
   * 
   * @param pvName the internal name of the possible value for a configuration option
   * 
   * @param bundle the ResourceBundle instance for the desired language
   * 
   * @return the structured data describing the possible value
   */
  protected ConfigurationOptionPossibleValueModel buildOptionPossibleValue(String identifier,
      String pvName, ResourceBundle bundle) {
    final String propertyType = "_opt_value_display_";
    StringBuilder buffer =
        new StringBuilder(identifier.length() + propertyType.length() + pvName.length());
    buffer.append(identifier).append(propertyType).append(pvName);
    return new ConfigurationOptionPossibleValueModel(pvName, bundle.getString(buffer.toString()));
  }

  /**
   * Obtains information about a grouping of configuration options for a masking provider. This
   * implementation populates the appropriate structure with translated information about the group
   * itself. Data on the individual options belonging to the group needs to be added to the returned
   * structure.
   * 
   * @param identifier the name of the masking provider about which information is required
   * 
   * @param groupname the internal name of the group of options. Note that this name is likely just
   *        used as a convention within the resource bundle and probably doesn't mean anything to
   *        the actual masking provider.
   * 
   * @param bundle the ResourceBundle instance for the desired language
   * 
   * @return the structured data describing the group of configuration options without the actual
   *         options added
   */
  protected ConfigurationOptionGroupModel buildOptionGroup(String identifier, String groupName,
      ResourceBundle bundle) {
    final String propertyType = "_group_display_";
    StringBuilder buffer =
        new StringBuilder(identifier.length() + propertyType.length() + groupName.length());
    buffer.append(identifier).append(propertyType).append(groupName);
    return new ConfigurationOptionGroupModel(groupName, bundle.getString(buffer.toString()));
  }

  /**
   * @param provider the provider about which information is required
   *  
   * @return <i>True</i> if the masking provider supports the common configuration options and
   * <i>False</i> if not
   */
  protected boolean supportsCommonOptions(MaskingProviderTypes provider) {
    return true;
  }
  
  /**
   * Builds metadata describing the common configuration options supported by most masking providers.
   * 
   * @param bundle the ResourceBundle instance for the desired language
   * 
   * @return a list of the metadata objects for each of the common common configuration options
   */
  protected List<ConfigurationOptionModel> getCommonOptions(ResourceBundle bundle) {
    List<ConfigurationOptionModel> options = new ArrayList<>();
    ConfigurationOptionModel option =
        buildOption("__base", "unexpectedInputHandling", bundle, OptionType.String, "NULL");
    List<ConfigurationOptionPossibleValueModel> possibleValues = new ArrayList<>();
    for (UnexpectedMaskingInputHandler handler : UnexpectedMaskingInputHandler.values()) {
      possibleValues.add(buildOptionPossibleValue("__base", handler.toString(), bundle));
    }
    option.setPossibleValues(possibleValues);
    options.add(option);
    options.add(
        buildOption("__base", "unexpectedInputReturnMessage", bundle, OptionType.String, "OTHER"));
    return options;
  }
}
