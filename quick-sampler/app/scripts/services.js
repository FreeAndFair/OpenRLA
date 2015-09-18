'use strict';

/* Services */

(function(){

    var samplerServices = angular.module('samplerApp.services', []);

    // Return the 'bigint' namespace defined by bigint.js.
    samplerServices.factory('bigint', ['$window',
      function bigintFactory($window){
        return $window.BigInt;
    }]);

    // Return a spellsInt() function.
    samplerServices.factory('spellsInt', [
      function spellsIntFactory(){
        // If value spells an integer, then return the integer.
        // Otherwise, return NaN.
        function spellsInt(value) {
            // parseInt() returns an integer or NaN.
            var n = parseInt(value, 10);
            // Comparing toString() lets us invalidate strings like "2.5" or "15px".
            if (isNaN(n) || (n.toString() !== value.toString())) {
                return NaN;
            }
            return n;
        }
        return spellsInt;
    }]);

    // Return an isBMP() function.
    samplerServices.factory('isBMP', [
      function isBMPFactory(){
        // Return whether the string consists only of BMP Unicode characters,
        // which are characters whose Unicode code point is smaller than
        // 65,536.  These are the characters that can be encoded as a
        // single character of a Javascript string and don't require
        // encoding with a pair of surrogate characters.
        //
        // See the following for more info:
        //  http://en.wikipedia.org/wiki/Plane_(Unicode)
        //  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt#Description
        function isBMP(s) {
            var code, i;
            for (i = 0; i < s.length; i += 1) {
                code = s.charCodeAt(i);
                if (0xD800 <= code && code <= 0xDBFF) {
                    // Then the character is a high surrogate.
                    // See the code here:
                    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt#Examples
                    return false;
                }
            }
            return true;
        }
        return isBMP;
    }]);

    // Return a Javascript object of test data.
    samplerServices.factory('getTests', ['$http',
      function getTestsFactory($http){
        function getTests(onResponse) {
          // The warning "unused" (W098) is for things like--
          // 'config' is defined but never used.
          // See also--
          // https://github.com/jshint/jshint/issues/1140
          /* jshint unused:false */
          $http.get('bower_components/rivest-sampler-tests/tests.json',
            {timeout: 1000}).
            success(function(data, status, headers, config) {
              onResponse(data);
            }).
            error(function(data, status, headers, config) {
              throw('error: ' + data);
            });
          /* jshint unused:true */
        }
        return getTests;
    }]);

    // Return a SHA-256 hash function.
    samplerServices.factory('sha256', [
      function sha256Factory(){
        function sha256(s) {
            // With 'TEXT', the jsSHA constructor defaults to encoding 'UTF8'.
            var shaObj = new jsSHA(s, 'TEXT');
            var hash = shaObj.getHash('SHA-256', 'HEX');
            return hash;
        }
        return sha256;
    }]);

    samplerServices.factory('toBigInt', ['bigint',
      function toBigIntFactory(bigint){
        function toBigInt(hexString) {
          // Evgenus's BigInt only accepts hex strings that are all upper-case.
          // https://github.com/Evgenus/BigInt/issues/5
          hexString = hexString.toUpperCase();
          return bigint.str2bigInt(hexString, 16, 1);
        }
        return toBigInt;
    }]);

    samplerServices.factory('bigMod', ['bigint', 'toBigInt',
      function bigModFactory(bigint, toBigInt){
        // Return the integer resulting from interpreting hexString as
        // a big integer in hexadecimal form and dividing.
        function bigMod(hexString, divisor) {
          var n = toBigInt(hexString);
          // modInt() returns an int and not a bigInt.
          return bigint.modInt(n, divisor);
        }
        return bigMod;
    }]);

    samplerServices.factory('getSample', ['bigMod', 'sha256',
      function getSampleFactory(bigMod, sha256){
        // Return the 0-based index of the nth sample item.
        //
        // For example, a return value of 0 means the first item in the
        // collection should be picked.  This function throws an error
        // if the return value would otherwise be NaN.
        //
        // Params:
        //   n: a whole number representing which sample item to return,
        //     e.g. 1 for the first item, 2 for the second item, etc.
        //   debug: an optional argument which is an array to which
        //     debug information will be stored.
        //
        function getSample(seed, totalSize, n, debug) {
            var hexHash = sha256(seed + ',' + n.toString());
            var value = bigMod(hexHash, totalSize);
            if (isNaN(value)) {
                throw 'drawing sample ' + n + ' from size ' + totalSize +
                      ' with seed "' + seed + '" did not return a number';
            }
            if (debug !== undefined) {
                debug.push(hexHash);
            }
            return value;
        }
        return getSample;
    }]);

    samplerServices.factory('getSamples', ['getSample',
      function getSamplesFactory(getSample){
        // Get samples, allowing duplicates.
        //
        // Returns an array of integers.
        //
        // Params:
        //   smallestItem: the smallest integer in the collection.
        //     Defaults to 1.
        //   debug: an array in which to store debug information.
        //
        function getSamples(seed, totalSize, sampleSize, smallestItem, debug) {
          var item,
              items = [];
          if (smallestItem === undefined) {
              smallestItem = 1;
          }
          for (var i = 1; i <= sampleSize; i++) {
              item = getSample(seed, totalSize, i, debug) + smallestItem;
              items.push(item);
          }
          return items;
        }
        return getSamples;
    }]);

    samplerServices.factory('getSamplesUnique', ['getSample',
      function getSamplesUniqueFactory(getSample){
        // Get samples, skipping duplicates.
        //
        // Params:
        //   smallestItem: the smallest integer in the collection.
        //     Defaults to 1.
        //
        // Returns an array of length two:
        //
        //   1) an array without duplicates, and
        //   2) the original raw array with duplicates.
        //
        // If more samples are requested than the given total, then the
        // function simply returns the maximum number possible.
        // In particular, it does not error out, etc.
        function getSamplesUnique(seed, totalSize, sampleSize, smallestItem, debug) {
          var item,
              items = [],
              uniqueItems = [],
              selectedItems = {};
          if (smallestItem === undefined) {
              smallestItem = 1;
          }
          // Prevent infinite execution.
          if (sampleSize > totalSize) {
              sampleSize = totalSize;
          }
          for (var i = 1, count = 0; count < sampleSize; i++) {
              // Since getSample() throws an error instead of returning NaN,
              // we do not have to worry about preventing an endless loop
              // caused by repeated NaN return values.
              item = getSample(seed, totalSize, i, debug) + smallestItem;
              items.push(item);
              if (!(item in selectedItems)) {
                  selectedItems[item] = true;
                  uniqueItems.push(item);
                  count++;
              }
          }
          return [uniqueItems, items];
        }
        return getSamplesUnique;
    }]);

    samplerServices.factory('leftPadder', [
      function leftPadderFactory(){
        // Return a function that left-pads strings to the given length
        // with the given character.
        function leftPadder(length, char) {
            var prefix, sliceArg;
            prefix = new Array(length + 1).join(char);
            sliceArg = -1 * length;

            function leftPad(s) {
                return (prefix + s).slice(sliceArg);
            }

            return leftPad;
        }
        return leftPadder;
    }]);

})();
