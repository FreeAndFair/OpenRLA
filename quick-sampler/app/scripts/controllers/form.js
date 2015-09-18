'use strict';

/* Controllers */

(function(){

    var errorMessages = {
        integerRequired: 'A whole number is required.',
        numberTooSmall: 'The number must be bigger than zero.',
        sampleCountTooLarge: 'The sample count must be smaller than the total count.',
        seedNotBMP: 'Random seeds with non-basic characters are not yet supported.',
        seedRequired: 'A random seed is required.',
        positiveIntegerRequired: 'A whole number bigger than zero is required.'
    };

    var samplerControllers = angular.module('samplerApp.controllers.form', [
        'samplerApp.services'
    ]);

    // Return an error object.
    function makeError(message, related) {
        return {
            message: message,
            related: related
        };
    }

    // TODO: move this to services.js?
    function chooseSamples(getSamplesUnique, parsed) {
        var hashes = [],
            sampleCount = parsed.sampleCount,
            seed = parsed.seed,
            smallestItem = parsed.smallestItem,
            totalCount = parsed.totalCount;

        var result = getSamplesUnique(seed, totalCount, sampleCount, smallestItem, hashes);

        var uniqueItems = result[0];
        var sortedItems = uniqueItems.concat();  // make a copy.
        sortedItems.sort(function(a, b) {
            return a - b;
        });

        return {
            all: result[1],
            hashes: hashes,
            sorted: sortedItems,
            unique: uniqueItems
        };
    }

    /**
     * @ngdoc function
     * @name samplerApp.controller:MainCtrl
     * @description
     * # MainCtrl
     * Controller of the samplerApp
     *
     * We do not perform form validation on ng-keyup, ng-change, or even
     * ng-blur.  This is because we take the philosophy that the user
     * should be able to continue working on the form undisturbed until
     * pressing the submit button, which sends the message that the user
     * is finished.
     *     However, we do clear error messages on ng-change because that
     * signifies that the user has started to correct the error.  We also
     * clear errors on related input elements whose error message could
     * potentially change as a result (e.g. the error message saying that
     * the sample count is larger than the total number of items).
     */
    samplerControllers.controller('MainCtrl', ['$log', '$scope', '$window',
      'getSamplesUnique', 'isBMP', 'leftPadder', 'spellsInt',
      function ($log, $scope, $window, getSamplesUnique, isBMP, leftPadder, spellsInt) {

        var form,
            input,
            output,
            parsed,
            parseFunctions;

        // The parse functions each return an object containing either
        // an error object or the parsed integer value.

        function parseSeed(inputValue) {
            var result = {};
            if (!inputValue) {
                result.error = makeError(errorMessages.seedRequired);
            } else if (!isBMP(inputValue)) {
                result.error = makeError(errorMessages.seedNotBMP);
            } else {
                result.value = inputValue;
            }
            return result;
        }

        function parseInteger(inputValue) {
            var result = {};
            var n = spellsInt(inputValue);
            if (isNaN(n)) {
                result.error = makeError(errorMessages.integerRequired);
            } else {
                result.value = n;
            }
            return result;
        }

        function parsePositiveInteger(inputValue) {
            var result = parseInteger(inputValue);
            if (result.error !== undefined) {
                result.error = makeError(errorMessages.positiveIntegerRequired);
            } else if (result.value < 1) {
                result.error = makeError(errorMessages.numberTooSmall);
            }
            return result;
        }

        function parseSampleCount(inputValue) {
            var result = parsePositiveInteger(inputValue);
            if ((result.error === undefined) &&
                (parsed.totalCount !== undefined) &&
                (result.value > parsed.totalCount)) {
                result.error = makeError(errorMessages.sampleCountTooLarge, ['totalCount']);
            }
            return result;
        }

        function setOutput(all, unique, sorted, debugInfo) {
            output.allItems = all;
            output.uniqueItems = unique;
            output.sortedItems = sorted;
            output.debug = debugInfo;
        }

        // Actions to take when an input element changes:
        //
        // 1) clear the result display,
        // 2) clear any associated errors,
        // 3) set the parsed value if no error, and
        // 4) return any error (or else null).
        //
        // Returns undefined or an error object.
        function onInput(inputLabel) {
            var errors = form.errors,
                parsed = form.parsed,
                parseInput = parseFunctions[inputLabel],
                relatedErrors = form.relatedErrors;

            form.showing = false;
            // Clear the result display since the input has changed.
            setOutput();
            // Clear the main error and any related errors.
            // (It's okay to delete if the property does not exist.)
            delete errors[inputLabel];
            var related = relatedErrors[inputLabel];
            if (related !== undefined) {
                for (var i = 0, len = related.length; i < len; i++) {
                    delete errors[related[i]];
                }
                // We only need to clear related errors once.
                delete relatedErrors[inputLabel];
            }

            var result = parseInput(form.input[inputLabel]);
            // The value will be undefined if parsing yielded an error.
            parsed[inputLabel] = result.value;

            return result.error;
        }

        // Return whether the form is error-free up to this point.
        // Also sets errors if there is a validation error.
        function checkInput(isOkay, inputLabel) {
            var relatedErrors = form.relatedErrors;
            var error = onInput(inputLabel);

            if (error !== undefined) {
                var errorText = error.message;
                var related = error.related;

                form.errors[inputLabel] = errorText;

                if (related !== undefined) {
                    for (var i = 0, len = related.length; i < len; i++) {
                        var relatedLabel = related[i];
                        if (relatedErrors[relatedLabel] === undefined) {
                            relatedErrors[relatedLabel] = [];
                        }
                        relatedErrors[relatedLabel].push(inputLabel);
                    }
                }
                isOkay = false;
            }

            return isOkay;
        }

        // Notes re: form validation & input values
        //
        // Note that with input type "number", the input value may already be
        // converted to a number and not be a string.
        //
        // For input type "number", an empty input element can result
        // in an input value of null.
        //
        // Moreover, if the user supplied "-1" and the input element has
        // min="1", then input[key] will be undefined (and spellsInt()
        // will return NaN) for at least some browsers.  Thus, we include
        // the full helpful error message if the parsed integer is NaN.

        // Populate the given object with parsed values, and return
        // whether the form validated without error.
        function validateForm() {
            var isOkay = true;

            isOkay = checkInput(isOkay, 'seed');
            isOkay = checkInput(isOkay, 'smallestItem');
            isOkay = checkInput(isOkay, 'totalCount');

            // We validate sampleCount after totalCount because validating
            // sampleCount depends on totalCount.
            isOkay = checkInput(isOkay, 'sampleCount');

            return isOkay;
        }

        function makeDebugInfo(parsed, result) {
            var allItems = result.all,
                hashes = result.hashes,
                item,
                line,
                lines,
                padIndex,
                padSample,
                parsedSeed = parsed.seed,
                parsedSmallestItem = parsed.smallestItem,
                parsedTotalCount = parsed.totalCount,
                sampleCount;

            sampleCount = allItems.length;

            padIndex = leftPadder(sampleCount.toString().length, ' ');
            padSample = leftPadder((parsedTotalCount - 1).toString().length, ' ');

            lines = [
                'Seed: "' + parsedSeed + '"',
                '      "' + encodeURI(parsedSeed) + '" (url-encoded)',
                ''
            ];

            for (var i = 0; i < sampleCount; i++) {
                item = allItems[i] - parsedSmallestItem;
                line = padIndex(i + 1) + '. ' + hashes[i] + ' % ' +
                       parsedTotalCount + ' = ' + padSample(item);
                lines.push(line);
            }

            return lines.join('\n');
        }

        // Initialize the models.
        form = {};
        output = {};
        parseFunctions = {
            sampleCount: parseSampleCount,
            seed: parseSeed,
            smallestItem: parseInteger,
            totalCount: parsePositiveInteger
        };

        input = {
            smallestItem: 1
        };

        form.showing = false;
        form.errors = {};
        form.input = input;
        form.parsed = {};
        form.relatedErrors = {};

        parsed = form.parsed;
        // We need to set the parsed value as well to make sure the
        // highest-item element gets updated if the total count is updated.
        onInput('smallestItem');

        form.onInputChange = onInput;

        form.submit = function() {
            var debugInfo,
                result;

            if (!validateForm()) {
                return;
            }
            result = chooseSamples(getSamplesUnique, parsed);
            debugInfo = makeDebugInfo(parsed, result);
            setOutput(result.all, result.unique, result.sorted, debugInfo);
            form.showing = true;
        };

        // Initialize the scope.
        $scope.form = form;
        $scope.output = output;

        $scope.highestItem = function() {
            var highest = parsed.smallestItem + parsed.totalCount - 1;
            if ((typeof highest !== 'number') || isNaN(highest)) {
                highest = '';
            }
            return highest;
        };

    }]);

})();
