'use strict';

describe('Controller: MainCtrl', function () {

  beforeEach(module('samplerApp.controllers.form'));

  var MainCtrl,
    scope;

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    MainCtrl = $controller('MainCtrl', {
      $scope: scope
    });
  }));

  describe('output', function() {

    beforeEach(function() {
    });

    it('should calculate the sample correctly', function() {
      var input = scope.form.input;
      input.seed = 'abcde';
      input.totalCount = 5;
      input.sampleCount = 2;
      scope.form.submit();
      expect(scope.output.allItems).toEqual([2, 2, 2, 3]);
      expect(scope.output.uniqueItems).toEqual([2, 3]);
    });

    it('should respect the lowest item field', function() {
      var input = scope.form.input;
      input.seed = 'abcde';
      input.totalCount = 5;
      input.sampleCount = 2;
      input.smallestItem = -1;
      scope.form.submit();
      expect(scope.output.allItems).toEqual([0, 0, 0, 1]);
      expect(scope.output.uniqueItems).toEqual([0, 1]);
    });

  });

  describe('totalCount input', function() {

    beforeEach(function() {
    });

    it('should display an error if empty', function() {
      scope.form.submit();
      expect(scope.form.errors.totalCount).toEqual('A whole number bigger than zero is required.');
    });

    it('should clear the "sample count too large" error if the input changes', function() {
      var form = scope.form;
      var errors = form.errors;
      var input = form.input;

      input.totalCount = 100;
      input.sampleCount = 10000;
      form.submit();

      expect(errors.sampleCount).not.toBeUndefined();
      form.onInputChange('totalCount');
      expect(errors.sampleCount).toBeUndefined();
    });

    it('should not clear the "invalid sample count" error if the input changes', function() {
      var form = scope.form;
      var errors = form.errors;
      var input = form.input;

      input.totalCount = 100;
      input.sampleCount = 'abc';
      form.submit();

      expect(errors.sampleCount).not.toBeUndefined();
      form.onInputChange('totalCount');
      expect(errors.sampleCount).not.toBeUndefined();
    });

  });

  describe('sampleCount input', function() {

    beforeEach(function() {
    });

    it('should display an error if sample count too large', function() {
      var form = scope.form;
      var errors = form.errors;
      var input = form.input;

      input.totalCount = 100;
      input.sampleCount = 1000;
      form.submit();

      expect(errors.sampleCount).toEqual('The sample count must be smaller than the total count.');
    });

  });

});
