<?php
/**
 * @file classes/Eve/Cache/Interface.class.php
 * @brief Base interface for all our caching engines.
 * @date 2011-02-09
 * @author Anthony Le Mansec
 */

/**
 * @addtogroup caching
 * Local caching of previously-executed queries.
 * @{
 */

/**
 * @brief base interface for all our caching engines.
 */
interface Eve_Cache_Interface {

    /**
     * @brief is caching engine enabled in config ?
     * @return boolean true if caching is enabled, false otherwise.
     */
    public function isEnabled();
    
    /**
     * @brief is given resource cached locally ?
     * @param string $resource_identifier md5 hash resource identifier.
     * @return boolean true if resource is cached AND cache is still valid.
     */
    public function isCached($resource_identifier);

    /**
     * @brief Caches given data under given resource_identifier.
     * @param string $resource_identifier md5 hash resource identifier.
     * @param mixed $data resource data to cache.
     * @return boolean true on successful caching, false otherwise.
     */
    public function doCache($resource_identifier, $data);

    /**
     * @brief returns cached resource.
     * @param string $resource_identifier md5 hash resource identifier.
     * @return mixed|boolean resource data or false on failure.
     */
    public function getCache($resource_identifier);


}

/** @} */

?>
