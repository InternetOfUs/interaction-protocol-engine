/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import eu.internetofus.common.components.ValidationErrorException;
import eu.internetofus.common.vertx.ModelsPageContext;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.PublishedNorm;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Unit test to increases coverage of the {@link NormsRepository}
 *
 * @see NormsRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(VertxExtension.class)
public class NormsRepositoryTest {

  /**
   * Verify that can not found a published norm because that returned by repository is not right.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundPublishedNormBecauseReturnedJsonObjectIsNotRight(final VertxTestContext testContext) {

    final NormsRepository repository = new NormsRepositoryImpl(null, null) {

      @Override
      public void searchPublishedNormObject(final String id, final Handler<AsyncResult<JsonObject>> searchHandler) {

        searchHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));

      }
    };

    repository.searchPublishedNorm("any identifier", testContext.failing(fail -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not store a published norm because that returned by repository is not right.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#storePublishedNorm(PublishedNorm, Handler)
   */
  @Test
  public void shouldNotStorePublishedNormBecauseReturnedJsonObjectIsNotRight(final VertxTestContext testContext) {

    final NormsRepository repository = new NormsRepositoryImpl(null, null) {

      @Override
      public void storePublishedNorm(final JsonObject publishednorm, final Handler<AsyncResult<JsonObject>> storeHandler) {

        storeHandler.handle(Future.succeededFuture(new JsonObject().put("key", "value")));
      }
    };

    repository.storePublishedNorm(new PublishedNorm(), testContext.failing(fail -> {
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not store a published norm because that returned by repository is not right.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#storePublishedNorm(PublishedNorm, Handler)
   */
  @Test
  public void shouldNotStorePublishedNormBecauseStoreFailed(final VertxTestContext testContext) {

    final Throwable cause = new IllegalArgumentException("Cause that can not be stored");
    final NormsRepository repository = new NormsRepositoryImpl(null, null) {

      @Override
      public void storePublishedNorm(final JsonObject publishednorm, final Handler<AsyncResult<JsonObject>> storeHandler) {

        storeHandler.handle(Future.failedFuture(cause));
      }

    };

    repository.storePublishedNorm(new PublishedNorm(), testContext.failing(fail -> {
      assertThat(fail).isEqualTo(cause);
      testContext.completeNow();
    }));

  }

  /**
   * Verify that can not update a published norm because that returned by repository is not right.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#searchPublishedNorm(String, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdatePublishedNormBecauseUpdateFailed(final VertxTestContext testContext) {

    final Throwable cause = new IllegalArgumentException("Cause that can not be updated");
    final NormsRepository repository = new NormsRepositoryImpl(null, null) {

      @Override
      public void updatePublishedNorm(final JsonObject publishednorm, final Handler<AsyncResult<Void>> updateHandler) {

        updateHandler.handle(Future.failedFuture(cause));
      }
    };

    repository.updatePublishedNorm(new PublishedNorm(), testContext.failing(fail -> {

      assertThat(fail).isEqualTo(cause);
      testContext.completeNow();
    }));

  }

  /**
   * Should not update community because can not convert to an object.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPageObject(eu.internetofus.common.vertx.ModelsPageContext, Handler)
   */
  @Test
  public void shouldFailRetrievePublishedNormsPageObjectWhenSearchFail(final VertxTestContext testContext) {

    final DummyNormsRepository repository = spy(new DummyNormsRepository());
    final var context = new ModelsPageContext();
    context.query = NormsRepository.createPublishedNormsPageQuery("name", "description", null, null, null, null);
    context.sort = NormsRepository.createPublishedNormsPageSort(Arrays.asList("name", "-description"));
    context.offset = 3;
    context.limit = 11;
    repository.retrievePublishedNormsPageObject(context, testContext.failing(error -> testContext.completeNow()));

    @SuppressWarnings("unchecked")
    final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
    verify(repository, timeout(30000).times(1)).retrievePublishedNormsPageObject(eq(context.query), eq(context.sort), eq(context.offset), eq(context.limit), searchHandler.capture());
    searchHandler.getValue().handle(Future.failedFuture("Not found"));

  }

  /**
   * Should not update community because the obtained object not match.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPage(ModelsPageContext, Handler)
   */
  @Test
  public void shouldFailRetrievePublishedNormsPageWhenObjectNotMatch(final VertxTestContext testContext) {

    final DummyNormsRepository repository = spy(new DummyNormsRepository());
    final var context = new ModelsPageContext();
    context.query = NormsRepository.createPublishedNormsPageQuery("name", "description", List.of("Keywords"), "publisherId", 0l, Long.MAX_VALUE);
    context.sort = NormsRepository.createPublishedNormsPageSort(Arrays.asList("-name", "description"));
    context.offset = 23;
    context.limit = 100;
    repository.retrievePublishedNormsPage(context, testContext.failing(error -> testContext.completeNow()));

    @SuppressWarnings("unchecked")
    final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
    verify(repository, timeout(30000).times(1)).retrievePublishedNormsPageObject(eq(context.query), eq(context.sort), eq(context.offset), eq(context.limit), searchHandler.capture());
    searchHandler.getValue().handle(Future.succeededFuture(new JsonObject().put("udefinedKey", "value")));

  }

  /**
   * Should not update community because the obtained object not found.
   *
   * @param testContext context that executes the test.
   *
   * @see NormsRepository#retrievePublishedNormsPage(ModelsPageContext, Handler)
   */
  @Test
  public void shouldFailRetrievePublishedNormsPageWhenObjectNotFound(final VertxTestContext testContext) {

    final DummyNormsRepository repository = spy(new DummyNormsRepository());
    final var context = new ModelsPageContext();
    context.query = NormsRepository.createPublishedNormsPageQuery("name", "description", List.of("Keywords"), "publisherId", null, Long.MAX_VALUE);
    context.sort = NormsRepository.createPublishedNormsPageSort(Arrays.asList("-name", "description"));
    context.offset = 23;
    context.limit = 100;
    repository.retrievePublishedNormsPage(context, testContext.failing(error -> testContext.completeNow()));

    @SuppressWarnings("unchecked")
    final ArgumentCaptor<Handler<AsyncResult<JsonObject>>> searchHandler = ArgumentCaptor.forClass(Handler.class);
    verify(repository, timeout(30000).times(1)).retrievePublishedNormsPageObject(eq(context.query), eq(context.sort), eq(context.offset), eq(context.limit), searchHandler.capture());
    searchHandler.getValue().handle(Future.failedFuture("Not found"));

  }

  /**
   * Should not create a sort query with bad parameters.
   */
  @Test
  public void shouldCreatePublishedNormsPageSortFail() {

    try {

      NormsRepository.createPublishedNormsPageSort(List.of("undefined"));
      fail("No validation error thrown");

    } catch (final ValidationErrorException ignored) {

    }

  }

  /**
   * Should create a sort query.
   *
   * @throws ValidationErrorException If the parameters are wrong.
   */
  @Test
  public void shouldCreatePublishedNormsPageSort() throws ValidationErrorException {

    final var sort = new JsonObject().put("name", 1).put("description", 1).put("keywords", -1).put("publisherId", 1).put("_lastUpdateTs", -1);
    final var params = new ArrayList<String>();
    params.add("name");
    params.add("+description");
    params.add("-keywords");
    params.add("publisherId");
    params.add("-publishedTime");
    assertThat(NormsRepository.createPublishedNormsPageSort(params)).isEqualTo(sort);

  }

  /**
   * Should fail create a sort query because duplicated property.
   */
  @Test
  public void shouldFailCreatePublishedNormsPageSort() {

    final var params = new ArrayList<String>();
    params.add("+publish");
    params.add("-publishTime");
    assertThatExceptionOfType(ValidationErrorException.class).isThrownBy(() -> NormsRepository.createPublishedNormsPageSort(params));

  }

}
